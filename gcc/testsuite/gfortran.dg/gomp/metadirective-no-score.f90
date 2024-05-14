! { dg-do compile { target x86_64-*-* } }
! { dg-additional-options "-foffload=disable" }

! This test is expected to fail with compile-time errors:
! "A trait-score cannot be specified in traits from the construct,
!  device or target_device trait-selector-sets."


subroutine f1 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
!$omp metadirective &
!$omp&  when (device={kind (score(5) : host)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f2 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
!$omp metadirective &
!$omp&  when (device={kind (host), arch (score(6) : x86_64), isa (avx512f)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f3 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
!$omp metadirective &
!$omp&  when (device={kind (host), arch (score(6) : x86_64), &
!$omp&		  isa (score(7): avx512f)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-3 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f4 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
  integer, parameter :: omp_initial_device = -1
!$omp metadirective &
!$omp&  when (target_device={device_num (score(42) : omp_initial_device), &
!$omp&			 kind (host)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-3 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f5 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
  integer, parameter :: omp_initial_device = -1
!$omp metadirective &
!$omp&  when (target_device={device_num(omp_initial_device), &
!$omp&			 kind (score(5) : host)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-2 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f6 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
  integer, parameter :: omp_initial_device = -1
!$omp metadirective &
!$omp&  when (target_device={device_num(omp_initial_device), kind (host), &
!$omp&			 arch (score(6) : x86_64), isa (avx512f)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-2 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine

subroutine f7 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(*)
  double precision :: s
  integer :: i
  integer, parameter :: omp_initial_device = -1
!$omp metadirective &
!$omp&  when (target_device={device_num(omp_initial_device), kind (host), &
!$omp&			 arch (score(6) : x86_64), &
!$omp&			 isa (score(7): avx512f)} &
!$omp&	: parallel do)
  ! { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-3 }
  do i = 1, n
    a(i) = a(i) * s;
  end do
end subroutine
