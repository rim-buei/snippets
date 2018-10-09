module Main where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let lt = filter (< x) xs
      gte = filter (>= x) xs
  in qsort lt ++ [x] ++ qsort gte

main :: IO ()
main = print $ qsort [1 .. 20]
