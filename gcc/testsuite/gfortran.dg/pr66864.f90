! { dg-do run }
! PR fortran/66864
!
program t
   implicit none
   real(8) x
   x = 2.0d0**26.5d0
   if (floor(x) /= 94906265) STOP 1
   if (floor(2.0d0**26.5d0)/= 94906265) STOP 2
   x = 777666555.6d0
   if (floor(x) /= 777666555) STOP 3
   if (floor(777666555.6d0) /= 777666555) STOP 4
   x = 2000111222.6d0
   if (floor(x) /= 2000111222) STOP 5
   if (floor(2000111222.6d0) /= 2000111222) STOP 6
end program t
