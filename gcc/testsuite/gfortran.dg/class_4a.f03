! { dg-do compile }
!
! Test the fix for PR41583, in which the different source files
! would generate the same 'vindex' for different class declared
! types.
!
! The test comprises class_4a, class_4b class_4c and class_4d.f03

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  type t
  end type t
end module m
! { dg-final { keep-modules "m" } }
