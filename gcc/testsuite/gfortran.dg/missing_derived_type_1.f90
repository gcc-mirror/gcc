! { dg-do compile }
! Tests the fix for PR29364, in which the the absence of the derived type
! 'nonexist' was not diagnosed.
!
! Contributed by Tobias Burnus  <tobias.burnus@physik.fu-berlin.de>
!
module test
  implicit none
  type epot_t
    integer :: c
    type(nonexist),pointer :: l ! { dg-error "has not been declared" }
  end type epot_t
end module test
! { dg-final { cleanup-modules "test" } }
