! { dg-do compile }
!
! Test the fix for PR41583, in which the different source files
! would generate the same 'vindex' for different class declared
! types.
!
! This file does nothing other than clean up the modules.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  type t
  end type t
end module m
! { dg-final { cleanup-modules "m m2" } }
