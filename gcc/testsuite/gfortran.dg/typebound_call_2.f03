! { dg-do run }

! FIXME: Remove -w after polymorphic entities are supported.
! { dg-options "-w" }

! Type-bound procedures
! Check calls with passed-objects.

MODULE m
  IMPLICIT NONE

  TYPE add
    INTEGER :: wrong
    INTEGER :: val
  CONTAINS
    PROCEDURE, PASS :: func => func_add
    PROCEDURE, PASS(me) :: sub => sub_add
  END TYPE add

  TYPE trueOrFalse
    LOGICAL :: val
  CONTAINS
    PROCEDURE, PASS :: swap
  END TYPE trueOrFalse

CONTAINS

  INTEGER FUNCTION func_add (me, x)
    IMPLICIT NONE
    TYPE(add) :: me
    INTEGER :: x
    func_add = me%val + x
  END FUNCTION func_add

  SUBROUTINE sub_add (res, me, x)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: res
    TYPE(add), INTENT(IN) :: me
    INTEGER, INTENT(IN) :: x
    res = me%val + x
  END SUBROUTINE sub_add

  SUBROUTINE swap (me1, me2)
    IMPLICIT NONE
    TYPE(trueOrFalse), INTENT(INOUT) :: me1, me2

    IF (.NOT. me1%val .OR. me2%val) THEN
      CALL abort ()
    END IF
    
    me1%val = .FALSE.
    me2%val = .TRUE.
  END SUBROUTINE swap

  ! Do the testing here, in the same module as the type is.
  SUBROUTINE test ()
    IMPLICIT NONE

    TYPE(add) :: adder
    TYPE(trueOrFalse) :: t, f

    INTEGER :: x

    adder%wrong = 0
    adder%val = 42
    IF (adder%func (8) /= 50) THEN
      CALL abort ()
    END IF

    CALL adder%sub (x, 8)
    IF (x /=  50) THEN
      CALL abort ()
    END IF

    t%val = .TRUE.
    f%val = .FALSE.

    CALL t%swap (f)
    CALL f%swap (t)

    IF (.NOT. t%val .OR. f%val) THEN
      CALL abort ()
    END IF
  END SUBROUTINE test

END MODULE m

PROGRAM main
  USE m, ONLY: test
  CALL test ()
END PROGRAM main

! { dg-final { cleanup-modules "m" } }
