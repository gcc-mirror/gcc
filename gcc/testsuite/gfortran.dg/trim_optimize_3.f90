! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! PR 47065 - replace trim with substring expressions.
program main
  character(len=10) :: a, b
  character(kind=4,len=10) :: a4, b4
  character(len=100) :: line
  a = 'bcd'
  b = trim(a) // 'x'
  if (b /= 'bcdx') call abort
  a4 = 4_"bcd"
  b4 = trim(a4) // 4_'x'
  if (b4 /= 4_'bcdx') call abort
end
! { dg-final { scan-tree-dump-times "string_len_trim" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
