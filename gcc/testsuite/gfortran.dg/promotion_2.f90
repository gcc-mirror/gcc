! { dg-do compile }
! { dg-options "-fdefault-real-8 -fexternal-blas -fdump-tree-original" }
!
! PR fortran/54463
!
! Contributed by Simon Reinhardt
!
program test
  implicit none
  real, dimension(3,3) :: A
  A = matmul(A,A)
end program test

! { dg-final { scan-tree-dump-times "sgemm_" 0 "original" } }
! { dg-final { scan-tree-dump-times "dgemm_" 1 "original" } }
