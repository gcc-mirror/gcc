! { dg-do run }
! { dg-options "-fdump-tree-original" }
program main
  implicit none
  character(len=4) :: c
  integer :: i
  integer :: k1, k2, k3, k4, k11, k22, k33, k44

  k1 = 1
  k2 = 2
  k3 = 3
  k4 = 4
  k11 = 1
  k22 = 2
  k33 = 3
  k44 = 4
  c = 'abcd'
  if (c(2:) /= c(k2:k4)) call abort
  if (c(k2:k4) /= c(k22:)) call abort
  if (c(2:3) == c(1:2)) call abort
  if (c(1:2) == c(2:3)) call abort
  if (c(k1:) == c(k2:)) call abort
  if (c(:3) == c(:k4)) call abort
  if (c(:k4) == c(:3)) call abort
  if (c(:k3) == c(:k44)) call abort
end program main

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 6 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcmp" 2 "original" } }

