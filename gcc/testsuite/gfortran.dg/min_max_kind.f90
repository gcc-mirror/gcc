! { dg-do run }
! { dg-options "-O2 -std=gnu" }
! Verify that the GNU extensions to MIN/MAX handle mixed kinds properly.

program p
  implicit none
  integer(1), parameter :: i1 = 1
  integer(2), parameter :: i2 = 2
  real(4),    parameter :: r4 = 4
  real(8),    parameter :: r8 = 8
  if (kind (min (i1, i2)) /= kind (i2)) stop 1
  if (kind (min (i2, i1)) /= kind (i2)) stop 2
  if (kind (min (r4, r8)) /= kind (r8)) stop 3
  if (kind (min (r8, r4)) /= kind (r8)) stop 4
end program p
