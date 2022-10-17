! { dg-do run }
program foo
   use ieee_arithmetic
   use iso_fortran_env
   implicit none

   ! This allows us to test REAL128 if it exists, and still compile
   ! on platforms were it is not present
   ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89639
   integer, parameter :: large = merge(real128, real64, real128 > 0)

   integer i, p
   real x
   x = 4
   i = 4

   p = int(ieee_scalb(real(x, real32), int(i, int8)))
   if (p /= 64) stop 1
   p = int(ieee_scalb(real(x, real64), int(i, int8)))
   if (p /= 64) stop 2
   p = int(ieee_scalb(real(x, large), int(i, int8)))
   if (p /= 64) stop 3

   p = int(ieee_scalb(real(x, real32), int(i, int16)))
   if (p /= 64) stop 4
   p = int(ieee_scalb(real(x, real64), int(i, int16)))
   if (p /= 64) stop 5
   p = int(ieee_scalb(real(x, large), int(i, int16)))
   if (p /= 64) stop 6

   p = int(ieee_scalb(real(x, real32), int(i, int32)))
   if (p /= 64) stop 7
   p = int(ieee_scalb(real(x, real64), int(i, int32)))
   if (p /= 64) stop 8
   p = int(ieee_scalb(real(x, large), int(i, int32)))
   if (p /= 64) stop 9

   p = int(ieee_scalb(real(x, real32), int(i, int64)))
   if (p /= 64) stop 10
   p = int(ieee_scalb(real(x, real64), int(i, int64)))
   if (p /= 64) stop 11
   p = int(ieee_scalb(real(x, large), int(i, int64)))
   if (p /= 64) stop 12

end program foo
