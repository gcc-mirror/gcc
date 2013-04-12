! { dg-do compile }
!
! PR fortran/56929
!
! Contributed by Damian Rouson
!
! Allocatable scalar corrays were mishandled (ICE)
!
module parent_coarray_component
  type parent
    real, allocatable :: dummy[:]
  end type
  type, extends(parent) :: child
  end type
contains
  subroutine do_something(this)
    class(child) this
  end 
end
