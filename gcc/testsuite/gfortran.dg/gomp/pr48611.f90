! PR tree-optimization/48611
! { dg-do compile }
! { dg-options "-Os -fopenmp -fexceptions -fno-tree-ccp -fno-tree-copy-prop" }

  integer, allocatable :: a(:)
  logical :: l
!$omp parallel private (a) reduction (.or.:l)
  do i = 1, 7
    a(:) = i
  end do
!$omp end parallel
end
