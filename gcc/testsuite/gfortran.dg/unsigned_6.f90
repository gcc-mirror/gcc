! { dg-do run }
! { dg-options "-funsigned" }
! Test the uint intrinsic.
program main
  implicit none
  integer :: i
  real :: r
  complex :: c
  if (1u /= uint(1)) error stop 1
  if (2u /= uint(2.0)) error stop 2
  if (3u /= uint((3.2,0.))) error stop 3

  i = 4
  if (uint(i) /= 4u) error stop 4
  r = 5.2
  if (uint(r) /= 5u) error stop 5
  c = (6.2,-1.2)
  if (uint(c) /= 6u) error stop 6

  if (uint(z'ff') /= 255u) error stop 7
end program main
