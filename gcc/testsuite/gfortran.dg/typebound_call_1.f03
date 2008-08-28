! { dg-do run }

! Type-bound procedures
! Check basic calls to NOPASS type-bound procedures.

MODULE m
  IMPLICIT NONE

  TYPE add
  CONTAINS
    PROCEDURE, NOPASS :: func => func_add
    PROCEDURE, NOPASS :: sub => sub_add
    PROCEDURE, NOPASS :: echo => echo_add
  END TYPE add

  TYPE mul
  CONTAINS
    PROCEDURE, NOPASS :: func => func_mul
    PROCEDURE, NOPASS :: sub => sub_mul
    PROCEDURE, NOPASS :: echo => echo_mul
  END TYPE mul

CONTAINS

  INTEGER FUNCTION func_add (a, b)
    IMPLICIT NONE
    INTEGER :: a, b
    func_add = a + b
  END FUNCTION func_add

  INTEGER FUNCTION func_mul (a, b)
    IMPLICIT NONE
    INTEGER :: a, b
    func_mul = a * b
  END FUNCTION func_mul

  SUBROUTINE sub_add (a, b, c)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a, b
    INTEGER, INTENT(OUT) :: c
    c = a + b
  END SUBROUTINE sub_add

  SUBROUTINE sub_mul (a, b, c)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a, b
    INTEGER, INTENT(OUT) :: c
    c = a * b
  END SUBROUTINE sub_mul

  SUBROUTINE echo_add ()
    IMPLICIT NONE
    WRITE (*,*) "Hi from adder!"
  END SUBROUTINE echo_add

  INTEGER FUNCTION echo_mul ()
    IMPLICIT NONE
    echo_mul = 5
    WRITE (*,*) "Hi from muler!"
  END FUNCTION echo_mul

  ! Do the testing here, in the same module as the type is.
  SUBROUTINE test ()
    IMPLICIT NONE

    TYPE(add) :: adder
    TYPE(mul) :: muler

    INTEGER :: x

    IF (adder%func (2, 3) /= 5 .OR. muler%func (2, 3) /= 6) THEN
      CALL abort ()
    END IF

    CALL adder%sub (2, 3, x)
    IF (x /= 5) THEN
      CALL abort ()
    END IF

    CALL muler%sub (2, 3, x)
    IF (x /= 6) THEN
      CALL abort ()
    END IF

    ! Check procedures without arguments.
    CALL adder%echo ()
    x = muler%echo ()
    CALL adder%echo
  END SUBROUTINE test

END MODULE m

PROGRAM main
  USE m, ONLY: test
  CALL test ()
END PROGRAM main

! { dg-final { cleanup-modules "m" } }
