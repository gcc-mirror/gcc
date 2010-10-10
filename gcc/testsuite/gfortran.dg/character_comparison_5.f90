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
  if (c // 'a' >= d // 'a') call abort
  if ('a' // c >= 'a' // d) call abort
end program main

! { dg-final { scan-tree-dump-times "gfortran_concat_string" 0 "original" } }
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

