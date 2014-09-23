! Don't compile this anywhere.
! { dg-do compile { target { lp64 && { ! lp64 } } } }
!
! Test the fix for PR41583, in which the different source files
! would generate the same 'vindex' for different class declared
! types.
!
! The test comprises class_4a, class_4b class_4c.f03
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m2
  use m
  type, extends(t) :: t2
  end type t2
end module m2
