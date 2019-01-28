! { dg-do compile }
! PR82009  [F08] ICE with block construct
MODULE sparse_matrix_csx_benchmark_utils
  IMPLICIT NONE
CONTAINS  
  SUBROUTINE sparse_matrix_csr_benchmark ( )
    WRITE(*,*) 'At*x: t'
    block
      integer, dimension(1), parameter :: idxs=[1]
      integer :: i, idx
      do i = 1, size(idxs)
         idx = idxs(i)
      enddo
    end block
  END SUBROUTINE sparse_matrix_csr_benchmark
  SUBROUTINE sparse_matrix_csc_benchmark ( )
    WRITE(*,*) 'An*x: t'
    block
      integer, dimension(1), parameter :: idxs=[1]
      integer :: i, idx
      do i = 1, size(idxs)
         idx = idxs(i)
      enddo
    end block
  END SUBROUTINE sparse_matrix_csc_benchmark
END MODULE sparse_matrix_csx_benchmark_utils
