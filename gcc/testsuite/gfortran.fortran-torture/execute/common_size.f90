! The size of common 'com1' should be 80, instead of 112.
program common_size
   real (kind=8) a(8)
   real (kind=8) b(5), c(5)
   common /com1/b,c
   equivalence (a(1), b(2))
   b = 100
   c = 200 
   if ((a (4) .ne. 100) .or. (a(5) .ne. 200)) call abort
end
