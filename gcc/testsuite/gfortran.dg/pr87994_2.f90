! { dg-do run }
! PR fortran/87994
program p
   real, parameter :: a = 1.0
   data b /a%kind/
   if (b /= kind(a)) stop 1
end
