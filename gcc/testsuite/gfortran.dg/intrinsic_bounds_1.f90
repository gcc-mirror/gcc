! { dg-do compile }
! PR 54633 - this used to be rejected
program main
  integer :: x(minval((/1/),mask=(/.TRUE./)))
  integer, parameter :: m = minval((/1/))
  integer :: y(minval((/1/),mask=(/.TRUE./)))
end
