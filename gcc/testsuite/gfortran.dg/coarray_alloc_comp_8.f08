! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Check that type conversion during caf_sendget_by_ref is done for components.

program main

  implicit none

  type :: mytype
    integer :: i
    integer :: i4 
    integer(kind=1) :: i1
    real :: r8
    real(kind=4) :: r4
    integer :: arr_i4(4)
    integer(kind=1) :: arr_i1(4)
    real :: arr_r8(4)
    real(kind=4) :: arr_r4(4)
  end type

  type T
    type(mytype), allocatable :: obj
  end type T

  type(T), save :: bar[*]
  integer :: i4, arr_i4(4)
  integer(kind=1) :: i1, arr_i1(4)
  real :: r8, arr_r8(4)
  real(kind=4) :: r4, arr_r4(4)

  bar%obj = mytype(42, 4, INT(1, 1), 8.0, REAL(4.0, 4), (/ 1,2,3,4 /), &
  &       INT((/ 5,6,7,8 /), 1), (/ 1.2,3.4,5.6,7.8 /), REAL( &
  &       (/ 8.7,6.5,4.3,2.1 /), 4))

  bar[1]%obj%i1 = bar[1]%obj%r4
  if (bar%obj%i1 /= 4) stop 1
  bar[1]%obj%i4 = bar[1]%obj%r8
  if (bar%obj%i4 /= 8) stop 2
  bar[1]%obj%arr_i1 = bar[1]%obj%arr_r4
  if (any(bar%obj%arr_i1 /= (/ 8,6,4,2 /))) stop 3
  bar[1]%obj%arr_i4 = bar[1]%obj%arr_r8
  if (any(bar%obj%arr_i4 /= (/ 1,3,5,7 /))) stop 4

  bar%obj%i1 = INT(1, 1)
  bar%obj%i4 = 4
  bar%obj%arr_i1 = INT((/ 5,6,7,8 /), 1)
  bar%obj%arr_i4 = (/ 1,2,3,4 /)
  bar[1]%obj%r4 = bar[1]%obj%i1
  if (abs(bar%obj%r4 - 1.0) > 1E-4) stop 5
  bar[1]%obj%r8 = bar[1]%obj%i4
  if (abs(bar%obj%r8 - 4.0) > 1E-6) stop 6
  bar[1]%obj%arr_r4 = bar[1]%obj%arr_i1
  if (any(abs(bar%obj%arr_r4 - REAL((/ 5,6,7,8 /), 4)) > 1E-4)) stop 7
  bar[1]%obj%arr_r8 = bar[1]%obj%arr_i4
  if (any(abs(bar%obj%arr_r8 - (/ 1,2,3,4 /)) > 1E-6)) stop 8
end program

