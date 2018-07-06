! { dg-do run }
! { dg-options "-fdump-tree-original" }
program main
  character (len=2) :: a, b
  character (kind=4,len=4) :: c,d
  a = 'ab'
  b = 'aa'
  if (a < b) STOP 1
  c = 4_"aaaa"
  d = 4_"aaab"
  if (c == d) STOP 2
  if (c > d) STOP 3
end program main
! { dg-final { scan-tree-dump-times "_gfortran_compare_string_char4" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcmp" 2 "original" } }
