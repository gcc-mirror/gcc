! { dg-do compile }

! Type-bound procedures
! Check correct type-bound operator definitions.

MODULE m
  IMPLICIT NONE

  TYPE t
    LOGICAL :: x
  CONTAINS
    PROCEDURE, PASS :: onearg
    PROCEDURE, PASS :: twoarg1
    PROCEDURE, PASS :: twoarg2
    PROCEDURE, PASS(me) :: assign_proc

    GENERIC :: OPERATOR(.BINARY.) => twoarg1, twoarg2
    GENERIC :: OPERATOR(.UNARY.) => onearg
    GENERIC :: ASSIGNMENT(=) => assign_proc
  END TYPE t

CONTAINS

  INTEGER FUNCTION onearg (me)
    CLASS(t), INTENT(IN) :: me
    onearg = 5
  END FUNCTION onearg

  INTEGER FUNCTION twoarg1 (me, a)
    CLASS(t), INTENT(IN) :: me
    INTEGER, INTENT(IN) :: a
    twoarg1 = 42
  END FUNCTION twoarg1

  INTEGER FUNCTION twoarg2 (me, a)
    CLASS(t), INTENT(IN) :: me
    REAL, INTENT(IN) :: a
    twoarg2 = 123
  END FUNCTION twoarg2

  SUBROUTINE assign_proc (me, b)
    CLASS(t), INTENT(OUT) :: me
    LOGICAL, INTENT(IN) :: b
    me%x = .NOT. b
  END SUBROUTINE assign_proc

END MODULE m

! { dg-final { cleanup-modules "m" } }
