subroutine test16
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(1)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test16

subroutine test17
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(2)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test17

subroutine test20
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test20
