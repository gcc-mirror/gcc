! { dg-do compile }
!
! PR 40089: Public type with public component which has a private type
!
! Original test case by Juergen Reuter <reuter@physik.uni-freiburg.de>
! Adapted by Janus Weil <janus@gcc.gnu.org>

module m

  implicit none
  private

  public :: public_t

  type :: private_t
    integer :: i
  end type

  type :: public_t
     type(private_t), pointer :: public_comp_with_private_type
     procedure(ifc) , nopass, pointer :: ppc
  end type

  abstract interface
     integer function ifc ()
     end function
  end interface

end module m

program test
use m
implicit none
type(public_t) :: x
integer :: j
j = x%ppc()
end

! { dg-final { cleanup-modules "m" } }

