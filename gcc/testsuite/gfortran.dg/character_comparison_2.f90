! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
program main
  implicit none
  character(len=4) :: c
  integer :: n
  integer :: i
  integer :: k1, k2
  common /foo/ i

  n = 0
  i = 0
  k1 = 1
  k2 = 3
  c = 'abcd'
  n = n + 1 ; if (c(1:2) == c(1:2)) call yes
  n = n + 1 ; if (c(k1:k2) >= c(k1:k2)) call yes
  n = n + 1 ; if (c(:2) <= c(1:2)) call yes
  n = n + 1 ; if (c(k2:) .eq. c(k2:4)) call yes
  n = n + 1 ; if (c(:) .ge. c) call yes
  n = n + 1 ; if (c .le. c) call yes
  if (c(1:2) /= c(1:2)) STOP 1
  if (c(k1:k2) > c(k1:k2)) STOP 2
  if (c(:2) < c(1:2)) STOP 3
  if (c(:) .ne. c) STOP 4
  if (c(:2) .gt. c(1:2)) STOP 5
  if (c(1:2) .lt. c(:2)) STOP 6
  if (n /= i) STOP 7
end program main

subroutine yes
  implicit none
  common /foo/ i
  integer :: i
  i = i + 1
end subroutine yes

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

