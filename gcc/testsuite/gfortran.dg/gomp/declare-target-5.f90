subroutine foo()
  !$omp declare target  to(foo) device_type(bar)  ! { dg-error "Expected HOST, NOHOST or ANY" }
end

subroutine bar()
  !$omp declare target  to(bar) device_type(nohost)
  !$omp declare target  to(bar) device_type(host)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
end

module mymod_one
  implicit none
  integer :: a, b, c, d, e ,f
  integer :: m, n, o, p, q, r
  common /block1/ m, n
  common /block2/ o, p
  common /block3/ q, r
  !$omp declare target  to(a) device_type(nohost)
  !$omp declare target  to(b) device_type(any)
  !$omp declare target  to(c) device_type(host)
  !$omp declare target  link(d) device_type(nohost)
  !$omp declare target  link(e) device_type(any)
  !$omp declare target  link(f) device_type(host)

  !$omp declare target  to(c) device_type(host)
  !$omp declare target  link(d) device_type(nohost)
end module

module mtest
  use mymod_one ! { dg-error "Cannot change attributes of USE-associated symbol" }
  implicit none

  !$omp declare target  to(a) device_type(any)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  to(b) device_type(host)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  to(c) device_type(nohost)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(d) device_type(host)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(e) device_type(nohost)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(f) device_type(any)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
end module

module mymod
  implicit none
  integer :: a, b, c, d, e ,f
  integer :: m, n, o, p, q, r
  common /block1/ m, n
  common /block2/ o, p
  common /block3/ q, r
  !$omp declare target  to(a) device_type(nohost)
  !$omp declare target  to(b) device_type(any)
  !$omp declare target  to(c) device_type(host)
  !$omp declare target  link(d) device_type(nohost)
  !$omp declare target  link(e) device_type(any)
  !$omp declare target  link(f) device_type(host)

  !$omp declare target  to(c) device_type(host)
  !$omp declare target  link(d) device_type(nohost)

  !$omp declare target  to(a) device_type(any)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  to(b) device_type(host)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  to(c) device_type(nohost)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(d) device_type(host)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(e) device_type(nohost)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
  !$omp declare target  link(f) device_type(any)  ! { dg-error "previous OMP DECLARE TARGET directive to a different DEVICE_TYPE" }
end
