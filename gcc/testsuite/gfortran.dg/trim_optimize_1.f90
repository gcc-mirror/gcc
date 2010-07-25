! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! PR 40628 - optimize unnecessary TRIMs on assignment
program main
  character(len=3) :: a
  character(len=4) :: b,c
  b = 'abcd'
  a = trim(b)
  c = trim(trim(a))
  if (a /= 'abc') call abort
  if (c /= 'abc') call abort
end program main

! { dg-final { scan-tree-dump-times "memmove" 2 "original" } }
! { dg-final { scan-tree-dump-times "string_trim" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
