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
  if (c(2:) /= c(k2:k4)) STOP 1
  if (c(k2:k4) /= c(k22:)) STOP 2
  if (c(2:3) == c(1:2)) STOP 3
  if (c(1:2) == c(2:3)) STOP 4
  if (c(k1:) == c(k2:)) STOP 5
  if (c(:3) == c(:k4)) STOP 6
  if (c(:k4) == c(:3)) STOP 7
  if (c(:k3) == c(:k44)) STOP 8
end program main

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 6 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcmp" 2 "original" } }

