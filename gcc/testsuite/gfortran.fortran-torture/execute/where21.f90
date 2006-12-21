! { dg-do run }
! Test fix for PR fortran/30207.
program a
  implicit none
  integer, parameter :: i(4) = (/ 1, 1, 1, 1 /)
  integer :: z(4) = (/ 1, 1, -1, -1 /)
  where(z < 0) z(:) = 1
  if (any(z /= i)) call abort
end program a
