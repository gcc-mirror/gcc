! { dg-do run }
! PR fortran/87994
program p
   real :: a, b
   data b /a%kind/
   if (b /= kind(a)) stop 1
end
