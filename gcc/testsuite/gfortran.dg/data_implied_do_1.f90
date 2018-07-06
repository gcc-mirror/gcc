! { dg-do run }
! Test of the patch for PR23232, in which implied do loop
! variables were not permitted in DATA statements.
! 
! Contributed by Roger Ferrer Ibáñez <rofi@ya.com> 
!
PROGRAM p
  REAL :: TWO_ARRAY (3, 3)
  INTEGER :: K, J
  DATA ((TWO_ARRAY (K, J), K = 1, J-1), J = 1, 3) /3 * 1.0/
  DATA ((TWO_ARRAY (K, J), K = J, 3), J = 1, 3) /6 * 2.0/
  if (any (reshape (two_array, (/9/)) &
      .ne. (/2.0,2.0,2.0,1.0,2.0,2.0,1.0,1.0,2.0/))) STOP 1
END PROGRAM

