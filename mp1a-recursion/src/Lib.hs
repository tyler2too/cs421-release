--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 _ = []
mytake n (x:xs) = 
    if n < 0 then []
    else x : mytake (n-1) xs

--- ### mydrop


mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs) = 
    if n <= 0 then (x:xs)
    else mydrop (n-1) xs



--- ### rev

rev :: [a] -> [a]
rev xs = revhelper xs []
revhelper :: [a] -> [a] -> [a]
revhelper [] acc  = acc
revhelper (x:xs) acc = revhelper (xs) (x : acc)


--- ### app

app :: [a] -> [a] -> [a]
app [] [] = []
app xs [] = xs
app [] ys = ys
app (x:xs) ys = x : app xs ys


--- ### inclist

inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x + 1 : inclist (xs)

--- ### sumlist

sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist (xs)

--- ### myzip

myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys 

--- ### addpairs

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = addpairshelper (myzip xs ys)

addpairshelper :: (Num a) => [(a,a)] -> [a]
addpairshelper [] = []
addpairshelper ((x,y):rest) = (x + y) : addpairshelper rest


--- ### ones

ones :: [Integer]
ones = 1 : ones

--- ### nats

nats :: [Integer]
nats = [0..]

--- ### fib

fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)



--- Set Theory
--- ----------

--- ### add

add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys)
  | x < y     = x : y : ys 
  | x == y    = y : ys
  | otherwise = y : add x ys


--- ### union

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys)
  | x < y     = x : union xs (y:ys)
  | x == y    = x : union xs ys
  | otherwise = y : union (x:xs) ys

--- ### intersect

intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
  | x < y     = intersect xs (y:ys)
  | x > y     = intersect (x:xs) ys
  | otherwise = x : intersect xs ys 


--- ### powerset

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =
  let ps = powerset xs
  in union ps (addAll x ps)

addAll :: Ord a => a -> [[a]] -> [[a]]
addAll _ [] = []
addAll x (s:ss) = add x s : addAll x ss



--- Higher Order Functions
--- ----------------------

--- ### inclist'

inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)


--- ### sumlist'

sumlist' :: Num a => [a] -> a
sumlist' = P.foldr (+) 0

