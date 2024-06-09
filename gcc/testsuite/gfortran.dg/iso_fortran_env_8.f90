! { dg-do run }
!
! Check for the new Fortran 2023 ISO_FORTRAN_ENV named constants

program test
  use iso_fortran_env
  implicit none

  ! These integer kinds are guaranteed on 
  integer(int8) :: i8
  integer(int16) :: i16
  integer(int32) :: i32
  integer(int64) :: i64

  logical(logical8) :: l8
  logical(logical16) :: l16
  logical(logical32) :: l32
  logical(logical64) :: l64

  ! We do not support REAL16 for now, but check it can
  ! still be used in specification expressions
  real(kind=max(real16, real32)) :: x

  if (logical8 /= int8) stop 1
  if (logical16 /= int16) stop 2
  if (logical32 /= int32) stop 3
  if (logical64 /= int64) stop 4

  ! We do not support REAL16 for now
  if (real16 /= -2) stop 101

end program test
