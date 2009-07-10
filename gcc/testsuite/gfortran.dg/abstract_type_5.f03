! { dg-do "compile" }

! Abstract Types.
! Check for correct handling of abstract-typed base object references.

MODULE m
  IMPLICIT NONE

  TYPE, ABSTRACT :: abstract_t
    INTEGER :: i
  CONTAINS
    PROCEDURE, NOPASS :: proc
    PROCEDURE, NOPASS :: func
  END TYPE abstract_t

  TYPE, EXTENDS(abstract_t) :: concrete_t
  END TYPE concrete_t

CONTAINS

  SUBROUTINE proc ()
    IMPLICIT NONE
    ! Do nothing
  END SUBROUTINE proc

  INTEGER FUNCTION func ()
    IMPLICIT NONE
    func = 1234
  END FUNCTION func

  SUBROUTINE test ()
    IMPLICIT NONE
    TYPE(concrete_t) :: obj

    ! These are ok.
    obj%abstract_t%i = 42
    CALL obj%proc ()
    PRINT *, obj%func ()

    ! These are errors (even though the procedures are not DEFERRED!).
    CALL obj%abstract_t%proc () ! { dg-error "is of ABSTRACT type" }
    PRINT *, obj%abstract_t%func () ! { dg-error "is of ABSTRACT type" }
  END SUBROUTINE test

END MODULE m
! { dg-final { cleanup-modules "m" } }
