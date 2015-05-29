! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
program main
  implicit none
  character(len=4) :: c
  integer :: n
  integer :: i
  common /foo/ i

  n = 0
  i = 0
  c = 'abcd'
  n = n + 1 ; if (c == c) call yes
  n = n + 1 ; if (c >= c) call yes
  n = n + 1 ; if (c <= c) call yes
  n = n + 1 ; if (c .eq. c) call yes
  n = n + 1 ; if (c .ge. c) call yes
  n = n + 1 ; if (c .le. c) call yes
  if (c /= c) call abort
  if (c > c) call abort
  if (c < c) call abort
  if (c .ne. c) call abort
  if (c .gt. c) call abort
  if (c .lt. c) call abort
  if (n /= i) call abort
end program main

subroutine yes
  implicit none
  common /foo/ i
  integer :: i
  i = i + 1
end subroutine yes

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

