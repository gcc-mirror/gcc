! { dg-do compile }
! Tests the fix for PR20867 in which implicit typing was not done within
! statement functions and so was not confirmed or not by subsequent
! type delarations.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
  REAL :: st1
  st1(I)=I**2
  REAL :: I ! { dg-error " already has basic type of INTEGER" }
  END


