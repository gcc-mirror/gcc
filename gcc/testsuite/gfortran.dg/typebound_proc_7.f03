! { dg-do compile }

! Type-bound procedures
! Tests that SEQUENCE and BIND(C) types do not allow a type-bound procedure
! section.

MODULE testmod
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE sequencet
    SEQUENCE
    INTEGER :: a, b
  CONTAINS ! { dg-error "SEQUENCE" }
    PROCEDURE, NOPASS :: proc_noarg
  END TYPE sequencet

  TYPE, BIND(C) :: bindct
    INTEGER(c_int) :: a
    REAL(c_float) :: b
  CONTAINS ! { dg-error "BIND" }
    PROCEDURE, NOPASS :: proc_noarg
  END TYPE bindct

CONTAINS

  SUBROUTINE proc_noarg ()
  END SUBROUTINE proc_noarg

END MODULE testmod
