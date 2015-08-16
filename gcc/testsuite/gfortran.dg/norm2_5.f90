! { dg-do compile }
!
! Testcase from PR 54656
! Checking for NORM2 for large float kinds
!
program test
  implicit none

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k1) :: d1(10), z1
  real(kind=k2) :: d2(10), z2
  d1 = 1 ; d2 = 1
  z1 = norm2 (d1)
  z2 = norm2 (d2)

  print *, z1, z2
end program test
