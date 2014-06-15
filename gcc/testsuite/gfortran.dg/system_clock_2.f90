! { dg-do compile }
! { dg-options "-std=f95" }

  integer :: i, j, k
  integer(kind=8) :: i8, j8, k8
  real :: x
  double precision :: z

  call system_clock(i, j, k)
  call system_clock(i, j, k8) ! { dg-error "has non-default kind" }
  call system_clock(i, j8, k) ! { dg-error "has non-default kind" }
  call system_clock(i8, j, k) ! { dg-error "has non-default kind" }

  call system_clock(i, x, k) ! { dg-error "Real COUNT_RATE argument" }

  call system_clock(i, z, k) ! { dg-error "Real COUNT_RATE argument" }

  end
