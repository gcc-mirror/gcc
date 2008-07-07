! PR middle-end/36726
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo
  integer, allocatable :: vs(:)
  !$omp parallel private (vs)
  allocate (vs(10))
  vs = 2
  deallocate (vs)
  !$omp end parallel
end subroutine foo
subroutine bar
  integer, allocatable :: vs(:)
  !$omp parallel private (vs)
  allocate (vs(10))
  vs = 2
  deallocate (vs)
  !$omp end parallel
end subroutine bar
