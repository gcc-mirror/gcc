! { dg-do compile }

! Type-bound procedures
! Check for correct access-checking on type-bound procedures.

MODULE m
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, NOPASS, PRIVATE :: priv => proc
    PROCEDURE, NOPASS, PUBLIC :: publ => proc
  END TYPE t

CONTAINS

  SUBROUTINE proc ()
  END SUBROUTINE proc

  ! This is inside the module.
  SUBROUTINE test1 ()
    IMPLICIT NONE
    TYPE(t) :: obj

    CALL obj%priv () ! { dg-bogus "PRIVATE" }
    CALL obj%publ ()
  END SUBROUTINE test1

END MODULE m

! This is outside the module.
SUBROUTINE test2 ()
  USE m
  IMPLICIT NONE
  TYPE(t) :: obj

  CALL obj%priv () ! { dg-error "PRIVATE" }
  CALL obj%publ ()
END SUBROUTINE test2

! { dg-final { cleanup-modules "m" } }
