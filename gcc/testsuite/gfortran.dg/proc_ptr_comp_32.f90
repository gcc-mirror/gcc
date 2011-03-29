! { dg-do compile }
!
! PR 48095: [OOP] Invalid assignment to procedure pointer component not rejected
!
! Contributed by Arjen Markus <arjen.markus895@gmail.com>

module m

  implicit none

  type :: rectangle
    procedure(get_area), pointer :: get_special_area
  end type rectangle

  abstract interface
    real function get_area( this )
      import                       :: rectangle
      class(rectangle), intent(in) :: this
    end function get_area
  end interface

contains

  real function get_my_area( this )
    type(rectangle), intent(in) :: this
    get_my_area = 3.0
  end function get_my_area

end module


use m
type(rectangle) :: rect
rect%get_special_area => get_my_area  ! { dg-error "Interface mismatch in procedure pointer assignment" }
end

! { dg-final { cleanup-modules "m" } }
