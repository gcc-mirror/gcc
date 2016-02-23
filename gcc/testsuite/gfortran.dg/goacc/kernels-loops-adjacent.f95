! { dg-additional-options "-O2" }

program main
   implicit none

   integer :: a(10000), b(10000)
   integer :: d

   !$acc kernels
   a = 1
   b = 2
   a = a + b
   !$acc end kernels

   d = sum(a)

   print *,d
end program main
