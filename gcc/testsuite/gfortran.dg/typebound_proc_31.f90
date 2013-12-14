! { dg-do compile }
!
! PR 59450: [OOP] ICE for type-bound-procedure expression in module procedure interface
!
! Contributed by <bugs@miller-mohr.de>

module classes

  implicit none

  type :: base_class
   contains
     procedure, nopass :: get_num
  end type

contains

  pure integer function get_num()
  end function

  function get_array( this ) result(array)
    class(base_class), intent(in) :: this
    integer, dimension( this%get_num() ) :: array
  end function

end module

! { dg-final { cleanup-modules "classes" } }
