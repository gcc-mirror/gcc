implicit none
integer :: iterator(10), i

!$omp taskgroup
  !$omp task affinity(iterator)
  !$omp end task

  !$omp task affinity(iterator(3))
  !$omp end task

  !$omp task affinity(iterator(i=1:10) : iterator(i))
  !$omp end task

  !$omp task affinity(iterator(integer :: i))  ! { dg-error "Failed to match clause at" }
  !!$omp end task

  !$omp task affinity(iterator(integer :: i=1:1))  ! { dg-error "Expected ':' at" }
  !!$omp end task

  !$omp task affinity(iterator(i=)) ! { dg-error "Expected range-specification at" }
!  !$omp end task
!$omp end taskgroup

end
