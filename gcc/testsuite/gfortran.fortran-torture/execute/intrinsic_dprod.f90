! Program to test DPROD intrinsic
program intrinsic_dprod
   implicit none
   real r, s, t
   double precision dp

   ! 6d60 doesn't fit in a 4-byte real
   r = 2e30
   s = 4e30
   dp = dprod (r, s)
   if ((dp .gt. 8.001d60) .or. (dp .lt. 7.999d60)) STOP 1
end program

