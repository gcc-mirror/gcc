! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! PR fortran/34162
! Internal procedures as actual arguments (like restricted closures).
! More challenging test involving recursion.

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

  ABSTRACT INTERFACE
    FUNCTION returnValue ()
      INTEGER :: returnValue
    END FUNCTION returnValue
  END INTERFACE

  PROCEDURE(returnValue), POINTER :: first

CONTAINS

  RECURSIVE SUBROUTINE test (level, current, previous)
    INTEGER, INTENT(IN) :: level
    PROCEDURE(returnValue), OPTIONAL :: previous, current

    IF (PRESENT (current)) THEN
      IF (current () /= level - 1) CALL abort ()
    END IF

    IF (PRESENT (previous)) THEN
      IF (previous () /= level - 2) CALL abort ()
    END IF

    IF (level == 1) THEN
      first => myLevel
    END IF
    IF (first () /= 1) CALL abort ()

    IF (level == 10) RETURN

    IF (PRESENT (current)) THEN
      CALL test (level + 1, myLevel, current)
    ELSE
      CALL test (level + 1, myLevel)
    END IF

  CONTAINS

    FUNCTION myLevel ()
      INTEGER :: myLevel
      myLevel = level
    END FUNCTION myLevel
    
  END SUBROUTINE test

END MODULE m

PROGRAM main
  USE :: m
  IMPLICIT NONE

  CALL test (1)
END PROGRAM main
