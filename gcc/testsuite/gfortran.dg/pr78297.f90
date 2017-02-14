! { dg-do compile }
module m
   real :: a(2), b(2)
   real :: c(2), d(2)
   equivalence (a, b)
   equivalence (c, d)
   common /xcom/ a
end
block data
   use m
end block data
