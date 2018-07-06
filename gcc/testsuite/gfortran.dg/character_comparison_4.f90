! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
program main
  implicit none
  character(len=4) :: c, d
  integer :: n
  integer :: i
  common /foo/ i

  n = 0
  i = 0
  c = 'abcd'
  d = 'efgh'

  n = n + 1 ; if ('a' // c == 'a' // c) call yes
  n = n + 1 ; if (c // 'a' == c // 'a') call yes
  n = n + 1; if ('b' // c > 'a' // d) call yes
  n = n + 1; if (c // 'b' > c // 'a') call yes

  if ('a' // c /= 'a' // c) STOP 1
  if ('a' // c // 'b' == 'a' // c // 'a') STOP 2
  if ('b' // c == 'a' // c) STOP 3
  if (c // 'a' ==  c // 'b') STOP 4
  if (c // 'a ' /=  c // 'a') STOP 5
  if (c // 'b' /=  c // 'b ') STOP 6

  if (n /= i) STOP 7
end program main

subroutine yes
  implicit none
  common /foo/ i
  integer :: i
  i = i + 1
end subroutine yes

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

