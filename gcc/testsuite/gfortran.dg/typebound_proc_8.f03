! { dg-do compile }

! Type-bound procedures
! Test for name collision between type-bound procedures and components.

MODULE testmod
  IMPLICIT NONE

  TYPE t
    REAL :: comp
  CONTAINS
    PROCEDURE, NOPASS :: comp => proc ! { dg-error "same name as a component" }
  END TYPE t

  TYPE supert
    INTEGER :: comp1
  CONTAINS
    PROCEDURE, NOPASS :: comp2 => proc
  END TYPE supert

  TYPE, EXTENDS(supert) :: subt1
    INTEGER :: comp2 ! { dg-error "same name" }
  END TYPE subt1

  TYPE, EXTENDS(supert) :: subt2
  CONTAINS
    PROCEDURE, NOPASS :: comp1 => proc ! { dg-error "same name as an inherited component" }
  END TYPE subt2

CONTAINS

  SUBROUTINE proc ()
  END SUBROUTINE proc

END MODULE testmod
