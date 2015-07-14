! { dg-do run }
! PR fortran/66864
!
program t
   implicit none
   real(8) x
   x = 2.0d0**26.5d0
   if (floor(x) /= 94906265) call abort
   if (floor(2.0d0**26.5d0)/= 94906265) call abort
   x = 777666555.6d0
   if (floor(x) /= 777666555) call abort
   if (floor(777666555.6d0) /= 777666555) call abort
   x = 2000111222.6d0
   if (floor(x) /= 2000111222) call abort
   if (floor(2000111222.6d0) /= 2000111222) call abort
end program t
