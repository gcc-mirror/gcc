subroutine foo (n)
  integer :: i, n
  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(source)		! { dg-error "Expected ':'" }
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(source:omp_current_iteration)	! { dg-error "Expected '\\\)' or 'omp_cur_iteration\\\)'" }
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(source:i - 2)	! { dg-error "Expected '\\\)' or 'omp_cur_iteration\\\)'" }
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink)		! { dg-error "Expected ':'" }
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:)		! { dg-error "Syntax error in OpenMP SINK dependence-type list" }
  end do
end

subroutine bar (n)
  implicit none
  integer i, n

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_current_iteration - 1)	! { dg-error "Symbol 'omp_current_iteration' at .1. has no IMPLICIT type" }
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_cur_iteration)	! { dg-error "omp_cur_iteration at .1. requires '-1' as logical offset" }
  end do
end

subroutine baz (n)
  implicit none
  integer i, n

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_cur_iteration + 1)	! { dg-error "omp_cur_iteration at .1. requires '-1' as logical offset" }
  end do
end

subroutine qux (n)
  implicit none
  integer i, n

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_cur_iteration - (2 - 1))	! { dg-error "Syntax error in OpenMP SINK dependence-type list" }
  end do
end

subroutine corge (n)
  implicit none
  integer i, n

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_cur_iteration - 1)
  end do

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(sink:omp_cur_iteration - 1_8)
  end do
end
