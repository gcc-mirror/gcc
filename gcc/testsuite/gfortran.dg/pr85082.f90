! { dg-do compile }
! { dg-options "-Ofast" }
program p
   real(4) :: a, b
   integer(4) :: n, m
   equivalence (a, n)
   a = 1024.0
   m = 8
   a = 1024.0
   b = set_exponent(a, m)
   n = 8
   a = f(a, n)
   b = set_exponent(a, m)
end
