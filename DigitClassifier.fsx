#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open System.IO
open FSharp.Charting

(******* 1. GETTING SOME DATA *******)
let dataLines = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\trainingsample.csv""").[1..]

(******* 2. EXTRACTING COLUMNS *******)
let dataNumbers = 
    dataLines 
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (Array.map (int)) 

(******* 3. CONVERTING ARRAYS TO RECORDS *******)
type DigitRecord = { Label:int; Pixels:int[] }

let dataRecords = 
    dataNumbers 
    |> Array.map (fun record -> {Label = record.[0]; Pixels = record.[1..]})

(******* 4. TRAINING vs VALIDATION DATA *******)
let trainingSet = dataRecords.[..3999]
let crossValidationSet = dataRecords.[4000..4499]
let testSet = dataRecords.[4500..]

(******* 5. COMPUTING DISTANCE *******)
let distanceTo (unknownDigit:int[]) (knownDigit:DigitRecord) = 
    Array.map2 (
        fun unknown known -> 
            let difference = unknown-known
            int64 (difference * difference)
        ) unknownDigit knownDigit.Pixels
    |> Array.sum

(******* 6. THE CLASSIFIER FUNCTION *******)
let classifyByNearest k (unknownDigit:int[]) =
    trainingSet
    |> Array.sortBy (distanceTo unknownDigit)
    |> Seq.take k
    |> Seq.countBy (fun digit -> digit.Label )
    |> Seq.maxBy (fun (label,count) -> count)
    |> fun (label,count) -> label

(******* 7. SEE THE CLASSIFIER IN ACTION *******)
testSet.[..4]
|> Array.iter (fun digit -> 
    printfn "Actual: %d, Predicted: %d" 
        digit.Label 
        (digit.Pixels |> classifyByNearest 3))

(******* 8. EVALUATING THE MODEL AGAINST VALIDATION DATA *******)
let calculateAccuracyWithNearest k =
    testSet
    |> Array.averageBy (fun digit -> 
        if digit.Pixels |> classifyByNearest k = digit.Label then 1.0 
        else 0.0)

let predictionAccuracy = 
    [1;3;9;27]
    |> List.map (fun k -> (k, calculateAccuracyWithNearest k))

Chart.Line predictionAccuracy
