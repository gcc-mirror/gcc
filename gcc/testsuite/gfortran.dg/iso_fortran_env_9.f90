! { dg-do compile }
! { dg-options "-std=f2018" }
!
! Check diagnostics for new F2023 named constants
! in ISO_FORTRAN_ENV
!

subroutine foo
  use iso_fortran_env
  implicit none
  logical(kind=logical8) :: x ! { dg-error "has no IMPLICIT type" }
end subroutine

subroutine bar
  use iso_fortran_env, only : logical8 ! { dg-error "not in the selected standard" }
  use iso_fortran_env, only : logical16 ! { dg-error "not in the selected standard" }
  use iso_fortran_env, only : logical32 ! { dg-error "not in the selected standard" }
  use iso_fortran_env, only : logical64 ! { dg-error "not in the selected standard" }
  use iso_fortran_env, only : real16 ! { dg-error "not in the selected standard" }
  implicit none
end subroutine

subroutine gee
  use iso_fortran_env, only : int8
  use iso_fortran_env, only : int16
  use iso_fortran_env, only : int32
  use iso_fortran_env, only : int64
  implicit none
end subroutine
