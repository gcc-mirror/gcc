! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }

! PR fortran/38936
! Check association and pointers.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, TARGET :: tgt
  INTEGER, POINTER :: ptr

  tgt = 1
  ASSOCIATE (x => tgt)
    ptr => x
    IF (ptr /= 1) CALL abort ()
    ptr = 2
  END ASSOCIATE
  IF (tgt /= 2) CALL abort ()
END PROGRAM main
