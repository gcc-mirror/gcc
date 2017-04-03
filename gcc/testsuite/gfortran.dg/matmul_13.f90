! { dg-do compile }
! { dg-options "-O3 -fdump-tree-optimized" }
! Check that the default limit of 30 for inlining matmul applies.
program main
  integer, parameter :: n = 31
  real, dimension(n,n) :: a, b, c
  call random_number(a)
  call random_number(b)
  c = matmul(a,b)
  print *,sum(c)
end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul_r4" 1 "optimized" } }
