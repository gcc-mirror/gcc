! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Check that type conversion during caf_send_by_ref is done for components.

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

  allocate(bar%obj)
  i1 = INT(1, 1)
  i4 = 4
  r4 = REAL(4.0, 4)
  r8 = 8.0
  arr_i1 = INT((/ 5,6,7,8 /), 1)
  arr_i4 = (/ 1,2,3,4 /)
  arr_r8 = (/ 1.2,3.4,5.6,7.8 /)
  arr_r4 = REAL((/ 8.7,6.5,4.3,2.1 /), 4)

  bar[1]%obj%r4 = i1
  if (abs(bar%obj%r4 - 1.0) > 1E-4) stop 1
  bar[1]%obj%r8 = i4
  if (abs(bar%obj%r8 - 4.0) > 1E-6) stop 2
  bar[1]%obj%i1 = r4
  if (bar%obj%i1 /= 4) stop 3
  bar[1]%obj%i4 = r8
  if (bar%obj%i4 /= 8) stop 4

  bar[1]%obj%arr_r4 = arr_i1
  print *, bar%obj%arr_r4
  if (any(abs(bar%obj%arr_r4 - REAL((/ 5,6,7,8 /), 4)) > 1E-4)) stop 5
  bar[1]%obj%arr_r8 = arr_i4
  if (any(abs(bar%obj%arr_r8 - (/ 1,2,3,4 /)) > 1E-6)) stop 6
  bar[1]%obj%arr_i1 = arr_r4
  if (any(bar%obj%arr_i1 /= INT((/ 8,6,4,2 /), 1))) stop 7
  bar[1]%obj%arr_i4 = arr_r8
  if (any(bar%obj%arr_i4 /= (/ 1,3,5,7 /))) stop 8
end program

