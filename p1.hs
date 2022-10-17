-- #1 Multiples of 3 or 5
m3or5 = sum [x | x <- [0..1000], x `mod` 3 == 0 || x `mod` 5 == 0]

-- #2 Even Fibonacci Numbers
fibn a b = a:fibn b (a+b)
fib = sum [x | x <- fst( span (<4000000) (fibn 0 1) ), x `mod` 2 == 0]
