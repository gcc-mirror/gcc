! { dg-do run }
! { dg-options "-w" }
! FIXME: Remove -w when CLASS is fully implemented.

! Type-bound procedures
! Check they can actually be called and run correctly.
! This also checks for correct module save/restore.

! FIXME: Check that calls to inherited bindings work once CLASS allows that.

MODULE m
  IMPLICIT NONE

  TYPE mynum
    REAL :: num_real
    INTEGER :: num_int
  CONTAINS
    PROCEDURE, PASS, PRIVATE :: add_mynum ! Check that this may be PRIVATE.
    PROCEDURE, PASS :: add_int
    PROCEDURE, PASS :: add_real
    PROCEDURE, PASS :: assign_int
    PROCEDURE, PASS :: assign_real
    PROCEDURE, PASS(from) :: assign_to_int
    PROCEDURE, PASS(from) :: assign_to_real
    PROCEDURE, PASS :: get_all

    GENERIC :: OPERATOR(+) => add_mynum, add_int, add_real
    GENERIC :: OPERATOR(.GET.) => get_all
    GENERIC :: ASSIGNMENT(=) => assign_int, assign_real, &
                                assign_to_int, assign_to_real
  END TYPE mynum

CONTAINS

  TYPE(mynum) FUNCTION add_mynum (a, b)
    CLASS(mynum), INTENT(IN) :: a, b
    add_mynum = mynum (a%num_real + b%num_real, a%num_int + b%num_int)
  END FUNCTION add_mynum

  TYPE(mynum) FUNCTION add_int (a, b)
    CLASS(mynum), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: b
    add_int = mynum (a%num_real, a%num_int + b)
  END FUNCTION add_int

  TYPE(mynum) FUNCTION add_real (a, b)
    CLASS(mynum), INTENT(IN) :: a
    REAL, INTENT(IN) :: b
    add_real = mynum (a%num_real + b, a%num_int)
  END FUNCTION add_real

  REAL FUNCTION get_all (me)
    CLASS(mynum), INTENT(IN) :: me
    get_all = me%num_real + me%num_int
  END FUNCTION get_all

  SUBROUTINE assign_real (dest, from)
    CLASS(mynum), INTENT(INOUT) :: dest
    REAL, INTENT(IN) :: from
    dest%num_real = from
  END SUBROUTINE assign_real

  SUBROUTINE assign_int (dest, from)
    CLASS(mynum), INTENT(INOUT) :: dest
    INTEGER, INTENT(IN) :: from
    dest%num_int = from
  END SUBROUTINE assign_int

  SUBROUTINE assign_to_real (dest, from)
    REAL, INTENT(OUT) :: dest
    CLASS(mynum), INTENT(IN) :: from
    dest = from%num_real
  END SUBROUTINE assign_to_real

  SUBROUTINE assign_to_int (dest, from)
    INTEGER, INTENT(OUT) :: dest
    CLASS(mynum), INTENT(IN) :: from
    dest = from%num_int
  END SUBROUTINE assign_to_int

  ! Test it works basically within the module.
  SUBROUTINE check_in_module ()
    IMPLICIT NONE
    TYPE(mynum) :: num

    num = mynum (1.0, 2)
    num = num + 7
    IF (num%num_real /= 1.0 .OR. num%num_int /= 9) CALL abort ()
  END SUBROUTINE check_in_module

END MODULE m

! Here we see it also works for use-associated operators loaded from a module.
PROGRAM main
  USE m, ONLY: mynum, check_in_module
  IMPLICIT NONE

  TYPE(mynum) :: num1, num2, num3
  REAL :: real_var
  INTEGER :: int_var

  CALL check_in_module ()

  num1 = mynum (1.0, 2)
  num2 = mynum (2.0, 3)

  num3 = num1 + num2
  IF (num3%num_real /= 3.0 .OR. num3%num_int /= 5) CALL abort ()

  num3 = num1 + 5
  IF (num3%num_real /= 1.0 .OR. num3%num_int /= 7) CALL abort ()

  num3 = num1 + (-100.5)
  IF (num3%num_real /= -99.5 .OR. num3%num_int /= 2) CALL abort ()

  num3 = 42
  num3 = -1.2
  IF (num3%num_real /= -1.2 .OR. num3%num_int /= 42) CALL abort ()

  real_var = num3
  int_var = num3
  IF (real_var /= -1.2 .OR. int_var /= 42) CALL abort ()

  IF (.GET. num1 /= 3.0) CALL abort ()
END PROGRAM main

! { dg-final { cleanup-modules "m" } }
