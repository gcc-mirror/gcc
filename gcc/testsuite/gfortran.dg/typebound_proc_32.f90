! { dg-do compile }
!
! PR 59547: [OOP] Problem with using tbp specification function in multiple class procedures
!
! Contributed by <bugs@miller-mohr.de>

module classes

  implicit none

  type :: base_class
   contains
     procedure, nopass :: get_num
     procedure :: get_array, get_array2
  end type

contains

  pure integer function get_num()
    get_num = 2
  end function

  function get_array( this ) result(array)
    class(base_class), intent(in) :: this
    integer, dimension( this%get_num() ) :: array
  end function

  function get_array2( this ) result(array)
    class(base_class), intent(in) :: this
    integer, dimension( this%get_num(), this%get_num() ) :: array
  end function

end module
