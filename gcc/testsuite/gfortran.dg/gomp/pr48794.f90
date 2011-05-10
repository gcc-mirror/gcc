! PR tree-optimization/48794
! { dg-do compile }
! { dg-options "-Os -fopenmp -fexceptions -fno-tree-ccp -fno-tree-copy-prop" }

  integer, allocatable :: a(:)
  logical :: l
  if (allocated (a)) call abort
!$omp parallel private (a) reduction (.or.:l)
  do i = 1, 7
  end do
!$omp end parallel
end
