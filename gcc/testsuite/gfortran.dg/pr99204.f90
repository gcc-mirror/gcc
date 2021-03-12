! PR tree-optimization/99204
! { dg-do compile }
! { dg-options "-O2 -w" }

program pr99204
  character :: c
  integer :: i = -12345678
  c = 'abc'(i:i)
  print *, c
end
