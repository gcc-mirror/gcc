! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Check that trailing trims are also removed from assignment of
! expressions involving concatenations of strings .
program main
  character(2) :: a,b,c
  character(8) :: d
  a = 'a '
  b = 'b '
  c = 'c '
  d = a // b // a // trim(c)   ! This should be optimized away.
  if (d /= 'a b a c ') STOP 1
  d = a // trim(b) // c // a   ! This shouldn't.
  if (d /= 'a bc a  ') STOP 2
  d = a // b // a // trim(trim(c)) ! This should also be optimized away.
  if (d /= 'a b a c ') STOP 3
end
! { dg-final { scan-tree-dump-times "string_len_trim" 1 "original" } }
