! { dg-do compile }
! { dg-options "-std=f2003 -fall-intrinsics" }

! PR fortran/38936
! Association to derived-type, where the target type is not know
! during parsing (only resolution).

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

  TYPE :: mynum
    INTEGER :: comp
  END TYPE mynum

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE add
  END INTERFACE OPERATOR(+)

CONTAINS

  PURE FUNCTION add (a, b)
    TYPE(mynum), INTENT(IN) :: a, b
    TYPE(mynum) :: add

    add%comp = a%comp + b%comp
  END FUNCTION add

END MODULE m

PROGRAM main
  USE :: m
  IMPLICIT NONE

  TYPE(mynum) :: a
  a = mynum (5)

  ASSOCIATE (x => add (a, a))
    IF (x%comp /= 10) CALL abort ()
  END ASSOCIATE

  ASSOCIATE (x => a + a)
    IF (x%comp /= 10) CALL abort ()
  END ASSOCIATE
END PROGRAM main
