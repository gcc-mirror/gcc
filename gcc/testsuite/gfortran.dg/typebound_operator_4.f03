! { dg-do compile }

! Type-bound procedures
! Check for errors with operator calls.

MODULE m
  IMPLICIT NONE

  TYPE myint
    INTEGER :: value
  CONTAINS
    PROCEDURE, PASS :: add_int
    PROCEDURE, PASS :: assign_int
    GENERIC, PRIVATE :: OPERATOR(.PLUS.) => add_int
    GENERIC, PRIVATE :: OPERATOR(+) => add_int
    GENERIC, PRIVATE :: ASSIGNMENT(=) => assign_int
  END TYPE myint

  TYPE myreal
    REAL :: value
  CONTAINS
    PROCEDURE, PASS :: add_real
    PROCEDURE, PASS :: assign_real
    GENERIC :: OPERATOR(.PLUS.) => add_real
    GENERIC :: OPERATOR(+) => add_real
    GENERIC :: ASSIGNMENT(=) => assign_real
  END TYPE myreal

CONTAINS

  PURE TYPE(myint) FUNCTION add_int (a, b)
    CLASS(myint), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b
    add_int = myint (a%value + b)
  END FUNCTION add_int

  PURE SUBROUTINE assign_int (dest, from)
    CLASS(myint), INTENT(OUT) :: dest
    INTEGER, INTENT(IN) :: from
    dest = myint (from)
  END SUBROUTINE assign_int

  TYPE(myreal) FUNCTION add_real (a, b)
    CLASS(myreal), INTENT(IN) :: a
    REAL, INTENT(IN) :: b
    add_real = myreal (a%value + b)
  END FUNCTION add_real

  SUBROUTINE assign_real (dest, from)
    CLASS(myreal), INTENT(OUT) :: dest
    REAL, INTENT(IN) :: from
    dest = myreal (from)
  END SUBROUTINE assign_real

  SUBROUTINE in_module ()
    TYPE(myint) :: x
    x = 0 ! { dg-bogus "Can't convert" }
    x = x + 42 ! { dg-bogus "Operands of" }
    x = x .PLUS. 5 ! { dg-bogus "Unknown operator" }
  END SUBROUTINE in_module

  PURE SUBROUTINE iampure ()
    TYPE(myint) :: x

    x = 0 ! { dg-bogus "is not PURE" }
    x = x + 42 ! { dg-bogus "to a non-PURE procedure" }
    x = x .PLUS. 5 ! { dg-bogus "to a non-PURE procedure" }
  END SUBROUTINE iampure

END MODULE m

PURE SUBROUTINE iampure2 ()
  USE m
  IMPLICIT NONE
  TYPE(myreal) :: x

  x = 0.0 ! { dg-error "is not PURE" }
  x = x + 42.0 ! { dg-error "to a non-PURE procedure" }
  x = x .PLUS. 5.0 ! { dg-error "to a non-PURE procedure" }
END SUBROUTINE iampure2

PROGRAM main
  USE m
  IMPLICIT NONE
  TYPE(myint) :: x

  x = 0 ! { dg-error "Can't convert" }
  x = x + 42 ! { dg-error "Operands of" }
  x = x .PLUS. 5 ! { dg-error "Unknown operator" }
END PROGRAM main

! { dg-final { cleanup-modules "m" } }
