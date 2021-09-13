! { dg-do compile }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:)
  integer i

  allocate (a(N))
  allocate (b(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1 2) ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1,) ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (,1) ! { dg-error "Invalid character in name" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1,2,) ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1,2 3) ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1,2,,) ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1  ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (*) ! { dg-error "Invalid character in name" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (a) ! { dg-error "ASYNC clause at \\\(1\\\) requires a scalar INTEGER expression" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (N)
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async (1.0) ! { dg-error "ASYNC clause at \\\(1\\\) requires a scalar INTEGER expression" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async () ! { dg-error "Invalid character in name at " }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) async
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel
end program asyncwait
