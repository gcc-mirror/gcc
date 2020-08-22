  integer :: i, j, k, l
  integer, dimension (10, 10) :: a
!$omp parallel do default (none) shared (a)
  do i = 1, 10
    j = 4
    do j = 1, 10
      a(i, j) = i + j
    end do
    j = 8
  end do
!$omp end parallel do
!$omp parallel default (none) shared (a) ! { dg-message "note: enclosing 'parallel'" }
  i = 1
  j = 1
  k = 1
  l = 1		! { dg-error "not specified in" }
  do i = 1, 10
    a(i, 1) = 1
  end do
!$omp critical
  do j = 1, 10
    a(1, j) = j
  end do
!$omp end critical
!$omp single
  do k = 1, 10
    a(k, k) = k
  end do
!$omp end single
!$omp end parallel
!$omp parallel default (none) shared (a) ! { dg-message "note: enclosing 'parallel'" }
  i = 1		! { dg-error "not specified in" }
  j = 1		! { dg-error "not specified in" }
  k = 1		! { dg-error "not specified in" }
!$omp parallel default (none) shared (a)
  i = 1
  j = 1
  k = 1
  do i = 1, 10
    a(i, 1) = 1
  end do
!$omp critical
  do j = 1, 10
    a(1, j) = j
  end do
!$omp end critical
!$omp single
  do k = 1, 10
    a(k, k) = k
  end do
!$omp end single
!$omp end parallel
  i = 1
  j = 1
  k = 1
!$omp end parallel
!$omp parallel default (none) shared (a) ! { dg-message "note: enclosing 'parallel'" }
  i = 1		! { dg-error "not specified in" }
!$omp do
  do i = 1, 10
    a(i, 1) = i + 1
  end do
!$omp end parallel
!$omp parallel default (none) shared (a) ! { dg-message "note: enclosing 'parallel'" }
  i = 1		! { dg-error "not specified in" }
!$omp parallel do default (none) shared (a)
  do i = 1, 10
    a(i, 1) = i + 1
  end do
!$omp end parallel
!$omp parallel default (none) shared (a) ! { dg-message "note: enclosing 'parallel'" }
  i = 1		! { dg-error "not specified in" }
!$omp parallel default (none) shared (a, i)
  i = 2
!$omp parallel default (none) shared (a)
  do i = 1, 10
    a(i, 1) = i
  end do
!$omp end parallel
  i = 3
!$omp end parallel
  i = 4
!$omp end parallel
end
