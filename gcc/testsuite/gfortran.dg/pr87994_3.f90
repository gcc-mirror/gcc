! { dg-do run }
! PR fortran/87994
program p
   integer, parameter :: a = 1
   integer :: b
   data b /a%kind/
   if (b /= kind(a)) stop = 1
end
