! PR tree-optimization/127100
! { dg-do compile { target s390*-*-* } }
! { dg-options "-O2 -march=z13 -fdump-tree-optimized" }
! Verify the test isn't miscompiled into unconditional STOP 1.
! { dg-final { scan-tree-dump "return 0;" "optimized" } }

program main
  integer(kind=1), dimension(2,2) :: a
  a = reshape((/ 1_1, 2_1, 3_1, 4_1/), shape(a))
  if (any(matmul(a,a) /= reshape ( (/ 7, 10, 15, 22 /), shape(a)))) STOP 1
end program main
