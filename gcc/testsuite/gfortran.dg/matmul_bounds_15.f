C { dg-do  run }
C { dg-options "-fdump-tree-optimized -fcheck=bounds -fblas-matmul-limit=1 -O -fexternal-blas" }
C { dg-shouldfail "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1.*" }
C { dg-additional-sources blas_gemm_routines.f }
      program main
      character(len=20) :: line
      integer :: n, m
      real, dimension(3,2) :: a
      real, dimension(:,:), allocatable :: b
      real, dimension(:,:), allocatable :: ret
      a = 1.0
      line = '3 3'
      read (unit=line,fmt=*) n, m
      allocate (b(n,m))
      b = 2.3
      ret = matmul(a,b)         ! This should throw an error.
      end
! { dg-output "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1.*" }
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
