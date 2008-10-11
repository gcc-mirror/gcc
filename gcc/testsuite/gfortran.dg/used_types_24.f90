! { dg-do compile }
! Tests the fix for PR37794 a regression where a bit of redundant code caused an ICE.
!
! Contributed by Jonathan Hogg  <J.Hogg@rl.ac.uk>
!
module m1
  implicit none

  type of01_data_private
    real :: foo
  end type of01_data_private

  type of01_data
    type (of01_data_private) :: private
  end type of01_data
end module m1

module m2
  implicit none

  type of01_data_private
    integer :: youngest
  end type of01_data_private
end module m2

module test_mod
  use m1, of01_rdata => of01_data
  use m2, of01_idata => of01_data ! { dg-error "not found in module" }

  implicit none
end module test_mod

! { dg-final { cleanup-modules "m1 m2 test_mod" } }
