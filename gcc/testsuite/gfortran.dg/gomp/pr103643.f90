! PR middle-end/103643
! { dg-do compile }

program test_task_affinity
  implicit none
  integer i
  integer, allocatable :: A(:)

  allocate (A(10))

  !$omp target
  !$omp task affinity(A)
  do i = 1, 10
     A(i) = 0
  end do
  !$omp end task
  !$omp end target

end program test_task_affinity
