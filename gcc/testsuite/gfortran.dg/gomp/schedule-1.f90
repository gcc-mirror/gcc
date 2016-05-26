  integer :: i
  !$omp do schedule(static, 1)
  do i = 1, 10
  end do
  !$omp do schedule(static, 0)	! { dg-warning "must be positive" }
  do i = 1, 10
  end do
  !$omp do schedule(static, -7)	! { dg-warning "must be positive" }
  do i = 1, 10
  end do
end
