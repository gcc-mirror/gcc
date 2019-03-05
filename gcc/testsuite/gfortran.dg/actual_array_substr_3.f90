! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR 43072 - no temporary needed because the substring
! is of equal length to the string.
subroutine foo2
  implicit none
  external foo
  character(len=20) :: str(2) = '1234567890'
  call foo(str(:)(1:20))
end
! { dg-final { scan-tree-dump-not "memmove" "original" } }
