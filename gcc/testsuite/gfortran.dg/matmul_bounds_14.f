C { dg-do  run }
C { dg-options "-fno-realloc-lhs -fdump-tree-optimized -fcheck=bounds -fblas-matmul-limit=1 -O -fexternal-blas" }
C { dg-shouldfail "Fortran runtime error: Array bound mismatch for dimension 2 of array." }
C { dg-additional-sources blas_gemm_routines.f }

      program main
      real, dimension(3,2) :: a
      real, dimension(2,3) :: b
      real, dimension(:,:), allocatable :: ret
      a = 1.0
      b = 2.3
      allocate(ret(3,2))
      ret = matmul(a,b)         ! This should throw an error.
      end
! { dg-output "Fortran runtime error: Array bound mismatch for dimension 2 of array.*" }
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
