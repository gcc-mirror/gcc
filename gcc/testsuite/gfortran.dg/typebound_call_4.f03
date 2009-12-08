! { dg-do compile }

! Type-bound procedures
! Check for recognition/errors with more complicated references and some
! error-handling in general.

MODULE m
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, PASS :: proc
    PROCEDURE, NOPASS :: func
  END TYPE t

  TYPE compt
    TYPE(t) :: myobj
  END TYPE compt

CONTAINS

  SUBROUTINE proc (me)
    IMPLICIT NONE
    CLASS(t), INTENT(INOUT) :: me
  END SUBROUTINE proc

  INTEGER FUNCTION func ()
    IMPLICIT NONE
    func = 1812
  END FUNCTION func

  SUBROUTINE test ()
    IMPLICIT NONE
    TYPE(compt) :: arr(2)

    ! These two are OK.
    CALL arr(1)%myobj%proc ()
    WRITE (*,*) arr(2)%myobj%func ()

    ! Can't CALL a function or take the result of a SUBROUTINE.
    CALL arr(1)%myobj%func () ! { dg-error "SUBROUTINE" }
    WRITE (*,*) arr(2)%myobj%proc () ! { dg-error "FUNCTION" }

    ! Error.
    CALL arr(2)%myobj%proc () x ! { dg-error "Junk after" }
    WRITE (*,*) arr(1)%myobj%func ! { dg-error "Expected argument list" }
  END SUBROUTINE test

END MODULE m

! { dg-final { cleanup-modules "m" } }
