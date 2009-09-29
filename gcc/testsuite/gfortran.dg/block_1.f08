! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! Basic Fortran 2008 BLOCK construct test.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: i

  i = 42

  ! Empty block.
  BLOCK
  END BLOCK

  ! Block without local variables but name.
  BLOCK
    IF (i /= 42) CALL abort ()
    i = 5
  END BLOCK
  IF (i /= 5) CALL abort ()

  ! Named block with local variable and nested block.
  myblock: BLOCK
    INTEGER :: i
    i = -1
    BLOCK
      IF (i /= -1) CALL abort ()
      i = -2
    END BLOCK
    IF (i /= -2) CALL abort ()
  END BLOCK myblock ! Matching end-label.
  IF (i /= 5) CALL abort ()
END PROGRAM main
