! { dg-do compile }
! Tests the fix for PR30876 in which interface derived types were
! not always being associated.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M1
CONTAINS
 FUNCTION correct_input(i)
   INTEGER :: i,correct_input(5), ans(5) = 0
   IF (i<1) correct_input=test(1)
   IF (i>5) correct_input=test(5)
 END FUNCTION correct_input

 RECURSIVE FUNCTION test(i)
  INTEGER :: test(5),i,j
  IF (i<1 .OR. i>5) THEN
    test=correct_input(i)
  ELSE
    test=0
    test(1:6-i)=(/(j,j=i,5)/)
    test=test(3)
  ENDIF
 END FUNCTION

END MODULE M1

USE M1
integer :: ans(5)
IF (ANY(TEST(3).NE.(/5,5,5,5,5/))) CALL ABORT()
IF (ANY(TEST(6).NE.(/0,0,0,0,0/))) CALL ABORT()
END
! { dg-final { cleanup-modules "m1" } }

