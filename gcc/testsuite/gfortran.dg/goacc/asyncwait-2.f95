! { dg-do compile }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:)
  integer i

  allocate (a(N))
  allocate (b(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1 2) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1,) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (,1) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1,2,) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1,2 3) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1,2,,) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1 ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (*) ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (a) ! { dg-error "WAIT clause at \\\(1\\\) requires a scalar INTEGER expression" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (N)
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait (1.0) ! { dg-error "WAIT clause at \\\(1\\\) requires a scalar INTEGER expression" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait () ! { dg-error "Syntax error in OpenACC expression list" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) waitasync ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) asyncwait ! { dg-error "Failed to match clause" }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel ! { dg-error "Unexpected \\\!\\\$ACC END PARALLEL" }

  !$acc parallel copyin (a(1:N)) copy (b(1:N)) wait
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel
end program asyncwait
