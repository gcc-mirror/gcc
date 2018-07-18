! { dg-do link }
! { dg-additional-sources "class_4b.f03 class_4c.f03" }
!
! Test the fix for PR41583, in which the different source files
! would generate the same 'vindex' for different class declared
! types.
!
! The test comprises class_4a, class_4b and class_4c.f03

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  type t
  end type t
end module m
