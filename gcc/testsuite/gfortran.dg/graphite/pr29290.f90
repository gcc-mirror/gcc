! PR tree-optimization/29290
! { dg-do compile }
! { dg-options "-O3 -ftree-loop-linear" }

subroutine pr29290 (a, b, c, d)
  integer c, d
  real*8 a(c,c), b(c,c)
  a(1:d,1:d) = b(1:d,1:d)
end
