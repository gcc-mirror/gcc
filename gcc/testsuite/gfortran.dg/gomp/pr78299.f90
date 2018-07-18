! PR fortran/78299
! { dg-do compile }
! { dg-additional-options "-fcheck=bounds" }

program pr78299
  integer, parameter :: n = 8
  integer :: i, j
  real :: x(n), y(n)
  x = 1.0
  y = 2.0
  do j = 1, 9
    !$omp parallel workshare
    !$omp parallel default(shared)
    !$omp do
    do i = 1, n
      x(i) = x(i) * y(9)		! { dg-warning "is out of bounds" }
    end do
    !$omp end do
    !$omp end parallel
    !$omp end parallel workshare
  end do
  do j = 1, 9
    !$omp parallel workshare
    !$omp parallel default(shared)
    !$omp do schedule(static)
    do i = 1, n
      x(i) = x(i) * y(9)		! { dg-warning "is out of bounds" }
    end do
    !$omp end do
    !$omp end parallel
    !$omp end parallel workshare
  end do
  do j = 1, 9
    !$omp parallel workshare
    !$omp parallel default(shared)
    !$omp do schedule(static, 2)
    do i = 1, n
      x(i) = x(i) * y(9)		! { dg-warning "is out of bounds" }
    end do
    !$omp end do
    !$omp end parallel
    !$omp end parallel workshare
  end do
  do j = 1, 9
    !$omp parallel workshare
    !$omp parallel default(shared)
    !$omp do schedule(dynamic, 3)
    do i = 1, n
      x(i) = x(i) * y(9)		! { dg-warning "is out of bounds" }
    end do
    !$omp end do
    !$omp end parallel
    !$omp end parallel workshare
  end do
end
