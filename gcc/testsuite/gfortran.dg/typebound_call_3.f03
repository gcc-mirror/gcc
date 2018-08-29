! { dg-do run }

! Type-bound procedures
! Check that calls work across module-boundaries.

MODULE m
  IMPLICIT NONE

  TYPE trueOrFalse
    LOGICAL :: val
  CONTAINS
    PROCEDURE, PASS :: swap
  END TYPE trueOrFalse

CONTAINS

  SUBROUTINE swap (me1, me2)
    IMPLICIT NONE
    CLASS(trueOrFalse), INTENT(INOUT) :: me1, me2

    IF (.NOT. me1%val .OR. me2%val) THEN
      STOP 1
    END IF
    
    me1%val = .FALSE.
    me2%val = .TRUE.
  END SUBROUTINE swap

END MODULE m

PROGRAM main
  USE m, ONLY: trueOrFalse
  IMPLICIT NONE

  TYPE(trueOrFalse) :: t, f

  t%val = .TRUE.
  f%val = .FALSE.

  CALL t%swap (f)
  CALL f%swap (t)

  IF (.NOT. t%val .OR. f%val) THEN
    STOP 2
  END IF
END PROGRAM main
