! { dg-do compile }
! { dg-options "-std=f95" }
! Check for notify-std-messages when F2003 structure constructors are compiled
! with -std=f95.

PROGRAM test
  IMPLICIT NONE

  ! Basic type with default initializers
  TYPE :: basics_t
    INTEGER :: i = 42
    REAL :: r = 1.5
  END TYPE basics_t

  TYPE(basics_t) :: basics

  ! This is ok in F95
  basics = basics_t (1, 2.)

  ! No argument naming in F95
  basics = basics_t (1, r = 4.2) ! { dg-error "Fortran 2003" }

  ! No optional arguments in F95
  basics = basics_t () ! { dg-error "Fortran 2003" }
  basics = basics_t (5) ! { dg-error "Fortran 2003" }

END PROGRAM test
