! { dg-do compile }
! { dg-options "-fdefault-real-8 -fexternal-blas -fblas-matmul-limit=1 -fdump-tree-original -finline-matmul-limit=0" }
!
! PR fortran/54463
!
! Contributed by Simon Reinhardt
!
program test
  implicit none
  real, dimension(3,3) :: A
  call random_number(a)
  A = matmul(A,A)
end program test

! { dg-final { scan-tree-dump-times "sgemm" 0 "original" } }
! { dg-final { scan-tree-dump-times "dgemm" 1 "original" } }
