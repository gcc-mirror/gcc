! { dg-do compile }
!
! PR 50227: [4.7 Regression] [OOP] ICE-on-valid with allocatable class variable
!
! Contributed by Andrew Benson <abenson@caltech.edu>

module G_Nodes
  private

  type, public :: t0
  end type
  
  type, public, extends(t0) :: t1
  end type

contains
  
  function basicGet(self)
    implicit none
    class(t0), pointer :: basicGet
    class(t0), target, intent(in) :: self
    select type (self)
    type is (t1)
       basicGet => self
    end select
  end function basicGet

end module G_Nodes
! { dg-final { keep-modules "" } }
