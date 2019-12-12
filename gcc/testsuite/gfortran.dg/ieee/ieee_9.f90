! { dg-do run { xfail arm*-*-gnueabi arm*-*-gnueabihf } }
! { dg-skip-if "PR89639" { hppa*-*-linux* } }
program foo
   use ieee_arithmetic
   use iso_fortran_env
   integer i, p
   real x
   x = 4
   i = 4

   if (int8 > 0) then
      if (real32 > 0) then
         p = int(ieee_scalb(real(x, real32), int(i, int8)))
         if (p /= 64) stop 1
      endif
      if (real64 > 0) then
         p = int(ieee_scalb(real(x, real64), int(i, int8)))
         if (p /= 64) stop 2
      endif
      if (real128 > 0) then
         p = int(ieee_scalb(real(x, real128), int(i, int8)))
         if (p /= 64) stop 3
      end if
   end if

   if (int16 > 0) then
      if (real32 > 0) then
         p = int(ieee_scalb(real(x, real32), int(i, int16)))
         if (p /= 64) stop 4
      endif
      if (real64 > 0) then
         p = int(ieee_scalb(real(x, real64), int(i, int16)))
         if (p /= 64) stop 5
      endif
      if (real128 > 0) then
         p = int(ieee_scalb(real(x, real128), int(i, int16)))
         if (p /= 64) stop 6
      end if
   end if

   if (int32 > 0) then
      if (real32 > 0) then
         p = int(ieee_scalb(real(x, real32), int(i, int32)))
         if (p /= 64) stop 7
      endif
      if (real64 > 0) then
         p = int(ieee_scalb(real(x, real64), int(i, int32)))
         if (p /= 64) stop 8
      endif
      if (real128 > 0) then
         p = int(ieee_scalb(real(x, real128), int(i, int32)))
         if (p /= 64) stop 9
      end if
   end if

   if (int64 > 0) then
      if (real32 > 0) then
         p = int(ieee_scalb(real(x, real32), int(i, int64)))
         if (p /= 64) stop 10
      endif
      if (real64 > 0) then
         p = int(ieee_scalb(real(x, real64), int(i, int64)))
         if (p /= 64) stop 11
      endif
      if (real128 > 0) then
         p = int(ieee_scalb(real(x, real128), int(i, int64)))
         if (p /= 64) stop 12
      end if
   end if

end program foo
