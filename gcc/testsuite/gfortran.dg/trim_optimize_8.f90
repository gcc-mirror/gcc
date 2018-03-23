! { dg-do compile }
! { dg-options "-O -fdump-tree-original" }
! Check that trailing trims are also removed from assignment of
! expressions involving concatenations of strings .
program main
  character(2) :: a,b
  character(8) :: d
  a = 'a '
  b = 'b '
  if (trim(a // trim(b)) /= 'a b ') STOP 1
  if (trim (trim(a) // trim(b)) /= 'ab ') STOP 2
end
! { dg-final { scan-tree-dump-times "string_len_trim" 1 "original" } }
