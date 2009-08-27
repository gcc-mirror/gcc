! { dg-do compile }
! { dg-options "-w" }
! FIXME: Remove -w once CLASS is fully supported.

! Type-bound procedures
! Checks for correct errors with invalid OPERATOR/ASSIGNMENT usage.

MODULE m
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, PASS :: onearg
    PROCEDURE, PASS :: onearg_alt => onearg
    PROCEDURE, PASS :: onearg_alt2 => onearg
    PROCEDURE, NOPASS :: nopassed => onearg
    PROCEDURE, PASS :: threearg
    PROCEDURE, PASS :: sub
    PROCEDURE, PASS :: sub2 ! { dg-error "must be a FUNCTION" }
    PROCEDURE, PASS :: func

    ! These give errors at the targets' definitions.
    GENERIC :: OPERATOR(.AND.) => sub2
    GENERIC :: OPERATOR(*) => onearg
    GENERIC :: ASSIGNMENT(=) => func

    GENERIC :: OPERATOR(.UOPA.) => sub ! { dg-error "must be a FUNCTION" }
    GENERIC :: OPERATOR(.UOPB.) => threearg ! { dg-error "at most, two arguments" }
    ! We can't check for the 'at least one argument' error, because in this case
    ! the procedure must be NOPASS and that other error is issued.  But of
    ! course this should be alright.

    GENERIC :: OPERATOR(.UNARY.) => onearg_alt
    GENERIC, PRIVATE :: OPERATOR(.UNARY.) => onearg_alt2 ! { dg-error "must have the same access" }

    GENERIC :: OPERATOR(.UNARYPRIME.) => nopassed ! { dg-error "can't be NOPASS" }
    GENERIC :: OPERATOR(-) => nopassed ! { dg-error "can't be NOPASS" }
  END TYPE t

CONTAINS

  INTEGER FUNCTION onearg (me) ! { dg-error "wrong number of arguments" }
    CLASS(t), INTENT(IN) :: me
    onearg = 5
  END FUNCTION onearg

  INTEGER FUNCTION threearg (a, b, c)
    CLASS(t), INTENT(IN) :: a, b, c
    threearg = 42
  END FUNCTION threearg

  LOGICAL FUNCTION func (me, b) ! { dg-error "must be a SUBROUTINE" }
    CLASS(t), INTENT(OUT) :: me
    CLASS(t), INTENT(IN) :: b
    me = t ()
    func = .TRUE.
  END FUNCTION func

  SUBROUTINE sub (a)
    CLASS(t), INTENT(IN) :: a
  END SUBROUTINE sub

  SUBROUTINE sub2 (a, x)
    CLASS(t), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: x
  END SUBROUTINE sub2

END MODULE m

! { dg-final { cleanup-modules "m" } }
