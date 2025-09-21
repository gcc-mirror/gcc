! { dg-do compile }
! { dg-options "-ffrontend-optimize -fexternal-blas64 -fdump-tree-original" }
! { dg-require-effective-target lp64 }
! PR 121161 - option for 64-bit BLAS for MATMUL.
! Check this by making sure there is no KIND=4 integer.
subroutine foo(a,b,c,n)
  implicit none
  integer(kind=8) :: n
  real, dimension(n,n) :: a, b, c
  c = matmul(a,b)
end subroutine foo
! { dg-final { scan-tree-dump-not "integer\\(kind=4\\)" "original" } }
! { dg-final { scan-tree-dump-times "sgemm" 1 "original" } }
