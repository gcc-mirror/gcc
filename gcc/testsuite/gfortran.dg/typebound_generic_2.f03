! { dg-do compile }

! Type-bound procedures
! Check for errors with calls to GENERIC bindings and their module IO.
! Calls with NOPASS.

MODULE m
  IMPLICIT NONE

  TYPE supert
  CONTAINS
    PROCEDURE, NOPASS :: func_int
    PROCEDURE, NOPASS :: sub_int
    GENERIC :: func => func_int
    GENERIC :: sub => sub_int
  END TYPE supert

  TYPE, EXTENDS(supert) :: t
  CONTAINS
    PROCEDURE, NOPASS :: func_real
    GENERIC :: func => func_real
  END TYPE t

CONTAINS

  INTEGER FUNCTION func_int (x)
    IMPLICIT NONE
    INTEGER :: x
    func_int = x
  END FUNCTION func_int

  INTEGER FUNCTION func_real (x)
    IMPLICIT NONE
    REAL :: x
    func_real = INT(x * 4.2)
  END FUNCTION func_real

  SUBROUTINE sub_int (x)
    IMPLICIT NONE
    INTEGER :: x
  END SUBROUTINE sub_int

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(t) :: myobj

  ! These are ok.
  CALL myobj%sub (1)
  WRITE (*,*) myobj%func (1)
  WRITE (*,*) myobj%func (2.5)

  ! These are not.
  CALL myobj%sub (2.5) ! { dg-error "no matching specific binding" }
  WRITE (*,*) myobj%func ("hello") ! { dg-error "no matching specific binding" }
  CALL myobj%func (2.5) ! { dg-error "SUBROUTINE" }
  WRITE (*,*) myobj%sub (1) ! { dg-error "FUNCTION" }

END PROGRAM main

! { dg-final { cleanup-modules "m" } }
