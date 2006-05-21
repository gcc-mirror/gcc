! { dg-do compile }
! This tests the patch for PR27584, where an ICE would ensue if
! a bad argument was fed for the target in ASSOCIATED.
!
! Contributed by Tobias Burnus  <tobias.burnus@physik.fu-berlin.de>
!
program test
   implicit none
   real, pointer :: x
   real, target :: y
   if(ASSOCIATED(X,(Y))) print *, 'Hello' ! { dg-error "VARIABLE or FUNCTION" }
end program test
