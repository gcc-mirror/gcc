! { dg-do run }
! { dg-options "-std=legacy" }
! PR 18540
! Verify that old-style cross-block GOTOs work
      I = 1
      GO TO 2
      IF (I .EQ. 0) THEN
 2       IF (I .NE. 1) STOP 1
         I = 0
         GOTO 3
      ELSE
 3       I = 2
      END IF
      IF (I .NE. 2) STOP 2
      END
