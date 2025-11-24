! { dg-do compile }
!
! Test the fix for PR12276.
! Exmple from F2018: C.2.5 Structure constructors and generic names
! Failed in each of the functions with, for example:
! "Derived type ‘pdtt_4’ at (1) is being used before it is defined"
! For each of the functions, if the function type was declared within
! the function, all was well.
!
MODULE m
  TYPE t(kind)
  INTEGER, KIND :: kind
  COMPLEX(kind) value
  END TYPE
  INTEGER,PARAMETER :: single = KIND(0.0), double = KIND(0d0)

  INTERFACE t
    MODULE PROCEDURE real_to_t1, dble_to_t2, int_to_t1, int_to_t2
  END INTERFACE

  CONTAINS
    TYPE(t(single)) FUNCTION real_to_t1(x)
    REAL(single) x
    real_to_t1%value = x
  END FUNCTION

  TYPE(t(double)) FUNCTION dble_to_t2(x)
    REAL(double) x
    dble_to_t2%value = x
  END FUNCTION
  TYPE(t(single)) FUNCTION int_to_t1(x,mold)
    INTEGER x
    TYPE(t(single)) mold
    int_to_t1%value = x
  END FUNCTION
  TYPE(t(double)) FUNCTION int_to_t2(x,mold)
    INTEGER x
    TYPE(t(double)) mold
      int_to_t2%value = x
  END FUNCTION

  END

  PROGRAM example
    USE m
    TYPE(t(single)) x
    TYPE(t(double)) y
    x = t(1.5) ! References real_to_t1
    print *, x%value
    x = t(17,mold=x) ! References int_to_t1
    print *, x%value
    y = t(1.5d0) ! References dble_to_t2
    print *, y%value
    y = t(42,mold=y) ! References int_to_t2
    print *, y%value
    y = t(kind(0d0)) ((0,1)) ! Uses the structure constructor for type t
    print *, y%value
  END
