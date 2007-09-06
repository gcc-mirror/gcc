! { dg-do run }
! PR 33298 - zero-sized arrays for spread were handled
!            incorrectly.

program main
  real :: x(0,3), y(0)
  x = spread(y,2,3)
end
