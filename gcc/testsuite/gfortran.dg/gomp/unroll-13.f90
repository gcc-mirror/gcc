subroutine foo
  integer :: i, j
  !$omp do collapse(2)
  do i = 1, 512
    !$omp unroll partial (3)
    do j = 1, 512
    end do
    !$omp end unroll
  end do
  !$omp end do
end subroutine foo

subroutine bar
  integer :: i, j
  !$omp do collapse(2)
  do i = 1, 512
    !$omp unroll partial (3)
    do j = 1, 512
    end do
  end do
  !$omp end do
end subroutine bar

subroutine baz
  integer :: i, j
  !$omp do collapse(2)
  do i = 1, 512
    !$omp unroll partial (3)
    do j = 1, 512
    end do
    !$omp end unroll
  end do
end subroutine baz

subroutine qux
  integer :: i, j
  !$omp do collapse(2)
  do i = 1, 512
    !$omp unroll partial (3)
    do j = 1, 512
    end do
  end do
end subroutine qux
