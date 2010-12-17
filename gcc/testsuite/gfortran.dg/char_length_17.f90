! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR 34145 - the length of the string should be simplified to one,
! no library call for string comparison is necessary.
program main
  character (len=5) :: c
  integer(kind=8) :: i
  i = 3
  c(i:i) = 'a'
  c(i+1:i+1) = 'b'
  if (c(i:i) /= 'a') call abort ()
  if (c(i+1:i+1) /= 'b') call abort ()
end program main
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
