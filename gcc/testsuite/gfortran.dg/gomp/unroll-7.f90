subroutine foo
  integer :: i
  !$omp do
  !$omp unroll partial ( 3 )
  do i = 1, 512
  end do
  !$omp end unroll
  !$omp end do
end subroutine foo

subroutine bar
  integer :: i
  !$omp do
  !$omp unroll partial(3)
  do i = 1, 512
  end do
  !$omp end do
end subroutine bar

subroutine baz
  integer :: i
  !$omp do
  !$omp unroll partial (3)
  do i = 1, 512
  end do
end subroutine baz

subroutine qux
  integer :: i
  !$omp do
  !$omp unroll partial (3)
  do i = 1, 512
  end do
  !$omp end unroll
end subroutine qux
