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

  if ('a' // c /= 'a' // c) call abort
  if ('a' // c // 'b' == 'a' // c // 'a') call abort
  if ('b' // c == 'a' // c) call abort
  if (c // 'a' ==  c // 'b') call abort
  if (c // 'a ' /=  c // 'a') call abort
  if (c // 'b' /=  c // 'b ') call abort

  if (n /= i) call abort
end program main

subroutine yes
  implicit none
  common /foo/ i
  integer :: i
  i = i + 1
end subroutine yes

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

