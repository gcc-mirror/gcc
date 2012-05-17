! { dg-do run }
!
! PR fortran/51869
!
module soop_stars_class
  implicit none
  type soop_stars
    real ,dimension(:,:) ,allocatable :: position
  end type
  type show
    type(soop_stars) :: rocket
  end type
contains
  function new_show(boom)
    type(soop_stars) ,intent(in) :: boom
    type(show) :: new_show
    new_show%rocket = boom
  end function
end module

program main
  use soop_stars_class
  implicit none

  type(soop_stars) :: fireworks
  type(show), allocatable :: july4

  allocate (fireworks%position(2,2))
  fireworks%position = 33.0

  july4 = new_show(boom=fireworks)
end program
