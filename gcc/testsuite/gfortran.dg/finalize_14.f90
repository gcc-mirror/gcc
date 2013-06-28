! { dg-do compile }
!
! PR fortran/37336
!
! Started to fail when finalization was added.
!
! Contributed by  Ian Chivers  in PR fortran/44465
! 
module shape_module

  type shape_type
    integer   :: x_=0
    integer   :: y_=0
    contains
    procedure , pass(this) :: getx
    procedure , pass(this) :: gety
    procedure , pass(this) :: setx
    procedure , pass(this) :: sety
    procedure , pass(this) :: moveto
    procedure , pass(this) :: draw
  end type shape_type

interface assignment(=)
  module procedure generic_shape_assign
end interface

contains

  integer function getx(this)
    implicit none
    class (shape_type) , intent(in) :: this
    getx=this%x_
  end function getx

  integer function gety(this)
    implicit none
    class (shape_type) , intent(in) :: this
    gety=this%y_
  end function gety

  subroutine setx(this,x)
    implicit none
    class (shape_type), intent(inout) :: this
    integer , intent(in) :: x
    this%x_=x
  end subroutine setx

  subroutine sety(this,y)
    implicit none
    class (shape_type), intent(inout) :: this
    integer , intent(in) :: y
    this%y_=y
  end subroutine sety

  subroutine moveto(this,newx,newy)
    implicit none
    class (shape_type), intent(inout) :: this
    integer , intent(in) :: newx
    integer , intent(in) :: newy
    this%x_=newx
    this%y_=newy
  end subroutine moveto

  subroutine draw(this)
    implicit none
    class (shape_type), intent(in) :: this
    print *,' x = ' , this%x_
    print *,' y = ' , this%y_
  end subroutine draw

  subroutine generic_shape_assign(lhs,rhs)
  implicit none
    class (shape_type) , intent(out) , allocatable :: lhs
    class (shape_type) , intent(in) :: rhs
      print *,' In generic_shape_assign'
      if ( allocated(lhs) ) then
        deallocate(lhs)
      end if
      allocate(lhs,source=rhs)
  end subroutine generic_shape_assign
  
end module shape_module

! Circle_p.f90

module circle_module

use shape_module

type , extends(shape_type) :: circle_type

  integer :: radius_

  contains

  procedure , pass(this) :: getradius
  procedure , pass(this) :: setradius
  procedure , pass(this) :: draw => draw_circle

end type circle_type

  contains

  integer function getradius(this)
  implicit none
  class (circle_type) , intent(in) :: this
    getradius=this%radius_
  end function getradius

  subroutine setradius(this,radius)
  implicit none
  class (circle_type) , intent(inout) :: this
  integer , intent(in) :: radius
    this%radius_=radius
  end subroutine setradius

  subroutine draw_circle(this)
  implicit none
    class (circle_type), intent(in) :: this
    print *,' x = ' , this%x_
    print *,' y = ' , this%y_
    print *,' radius = ' , this%radius_
  end subroutine draw_circle

end module circle_module


! Rectangle_p.f90

module rectangle_module

use shape_module

type , extends(shape_type) :: rectangle_type

  integer :: width_
  integer :: height_

  contains

  procedure , pass(this) :: getwidth
  procedure , pass(this) :: setwidth
  procedure , pass(this) :: getheight
  procedure , pass(this) :: setheight
  procedure , pass(this) :: draw => draw_rectangle

end type rectangle_type

  contains

  integer function getwidth(this)
  implicit none
  class (rectangle_type) , intent(in) :: this
    getwidth=this%width_
  end function getwidth

  subroutine setwidth(this,width)
  implicit none
  class (rectangle_type) , intent(inout) :: this
  integer , intent(in) :: width
    this%width_=width
  end subroutine setwidth

  integer function getheight(this)
  implicit none
  class (rectangle_type) , intent(in) :: this
    getheight=this%height_
  end function getheight

  subroutine setheight(this,height)
  implicit none
  class (rectangle_type) , intent(inout) :: this
  integer , intent(in) :: height
    this%height_=height
  end subroutine setheight

  subroutine draw_rectangle(this)
  implicit none
    class (rectangle_type), intent(in) :: this
    print *,' x = ' , this%x_
    print *,' y = ' , this%y_
    print *,' width = ' , this%width_
    print *,' height = ' , this%height_

  end subroutine draw_rectangle

end module rectangle_module



program polymorphic

use shape_module
use circle_module
use rectangle_module

implicit none

type shape_w
  class (shape_type) , allocatable :: shape_v
end type shape_w

type (shape_w) , dimension(3) :: p

  print *,' shape '

  p(1)%shape_v=shape_type(10,20)
  call p(1)%shape_v%draw()

  print *,' circle '

  p(2)%shape_v=circle_type(100,200,300)
  call p(2)%shape_v%draw()

  print *,' rectangle '

  p(3)%shape_v=rectangle_type(1000,2000,3000,4000)
  call p(3)%shape_v%draw()

end program polymorphic
