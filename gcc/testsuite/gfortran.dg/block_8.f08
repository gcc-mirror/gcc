! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! Check BLOCK with SAVE'ed variables.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: i

  DO i = 1, 100
    BLOCK
      INTEGER, SAVE :: summed = 0
      summed = summed + i
      IF (i == 100 .AND. summed /= 5050) CALL abort ()
    END BLOCK
  END DO
END PROGRAM main
