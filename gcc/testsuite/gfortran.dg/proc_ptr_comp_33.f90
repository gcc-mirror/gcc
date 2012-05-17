! { dg-do compile }
!
! PR 48095: [OOP] Invalid assignment to procedure pointer component not rejected
!
! Original test case by Arjen Markus <arjen.markus895@gmail.com>
! Modified by Janus Weil <janus@gcc.gnu.org>

module m

  implicit none

  type :: rectangle
    real :: width, height
    procedure(get_area_ai), pointer :: get_area => get_my_area  ! { dg-error "Type/rank mismatch" }
  end type rectangle

  abstract interface
    real function get_area_ai( this )
      import                       :: rectangle
      class(rectangle), intent(in) :: this
    end function get_area_ai
  end interface

contains

  real function get_my_area( this )
    type(rectangle), intent(in) :: this
    get_my_area = 3.0 * this%width * this%height
  end function get_my_area

end

!-------------------------------------------------------------------------------

program p

  implicit none

  type :: rectangle
    real :: width, height
    procedure(get_area_ai), pointer :: get_area
  end type rectangle

  abstract interface
    real function get_area_ai (this)
      import                       :: rectangle
      class(rectangle), intent(in) :: this
    end function get_area_ai
  end interface

  type(rectangle) :: rect

  rect  = rectangle (1.0, 2.0, get1)
  rect  = rectangle (3.0, 4.0, get2)  ! { dg-error "Type/rank mismatch" }

contains

  real function get1 (this)
    class(rectangle), intent(in) :: this
    get1 = 1.0 * this%width * this%height
  end function get1

  real function get2 (this)
    type(rectangle), intent(in) :: this
    get2 = 2.0 * this%width * this%height
  end function get2

end
