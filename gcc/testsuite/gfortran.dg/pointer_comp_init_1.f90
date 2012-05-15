! { dg-do compile }
!
! PR fortran/50050
! ICE whilst trying to access NULL shape.

! Reduced from the FoX library http://www1.gly.bris.ac.uk/~walker/FoX/
! Contributed by Andrew Benson <abenson@its.caltech.edu>

module m_common_attrs
  implicit none

  type dict_item
  end type dict_item

  type dict_item_ptr
     type(dict_item), pointer :: d => null()
  end type dict_item_ptr

contains

  subroutine add_item_to_dict()
    type(dict_item_ptr), pointer :: tempList(:)
    integer :: n

    allocate(tempList(0:n+1)) 
  end subroutine add_item_to_dict

end module m_common_attrs
