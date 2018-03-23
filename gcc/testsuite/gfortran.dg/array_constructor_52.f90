! { dg-do  run }
! PR 84931 - long array constructors with type conversion were not
! handled correctly.
program test
   implicit none
   integer, parameter :: n = 2**16
   real, dimension(n) :: y
   integer :: i
   y = (/ (1, i=1, n) /)
   if (y(2) /= 1) stop 1
end program test
