! { dg-do compile }
!
! PR fortran/51754
! This program was leading to an ICE related to class arrays
!
! Contributed by Andrew Benson <abenson@caltech.edu>

module test
  private

  type :: componentB
  end type componentB

  type :: treeNode
     class(componentB), allocatable, dimension(:) :: componentB
  end type treeNode

contains

  function BGet(self)
    implicit none
    class(componentB), pointer :: BGet
    class(treeNode), target, intent(in) :: self
    select type (self)
    class is (treeNode)
       BGet => self%componentB(1)
    end select
    return
  end function BGet

end module test
