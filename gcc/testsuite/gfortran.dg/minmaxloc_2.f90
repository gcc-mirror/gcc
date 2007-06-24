! { dg-do run }
! Tests the fix for PR32298, in which the scalarizer would generate
! a temporary in the course of evaluating MINLOC or MAXLOC, thereby
! setting the start of the scalarizer loop to zero.
!
! Contributed by Jens Bischoff <jens.bischoff@freenet.de> 
!
PROGRAM ERR_MINLOC

   INTEGER, PARAMETER :: N = 7

   DOUBLE PRECISION, DIMENSION (N), PARAMETER :: A &
     = (/ 0.3D0, 0.455D0, 0.6D0, 0.7D0, 0.72D0, 0.76D0, 0.79D0 /)

   DOUBLE PRECISION :: B
   INTEGER          :: I, J(N), K(N)

  DO I = 1, N
    B = A(I)
    J(I) = MINLOC (ABS (A - B), 1)
    K(I) = MAXLOC (ABS (A - B), 1)
  END DO

  if (any (J .NE. (/1,2,3,4,5,6,7/))) call abort ()
  if (any (K .NE. (/7,7,1,1,1,1,1/))) call abort ()

  STOP

END PROGRAM ERR_MINLOC
