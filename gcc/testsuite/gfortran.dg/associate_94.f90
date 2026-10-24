! { dg-do compile }
! PR fortran/126968
! Components of an associate name whose type was guessed wrongly ('g*_t')
! must be checked against the real type ('r*_t').
!
module m
  type :: g_t
     integer :: x = 1
  end type
  type :: r_t
     integer, private :: x = 2
  end type
  type :: g2_t
     integer :: v(3) = 9
  end type
  type :: r2_t
     integer :: v = 5
  end type
contains
  function make () result (y)
    type(r_t) :: y
  end function
end module

program p
  use m
  implicit none
  integer :: i
  associate (q => fcn ())
    i = q%x       ! { dg-error "is a PRIVATE component of" }
  end associate
  associate (q => fcn2 ())
    i = q%v(2)    ! { dg-error "Invalid array reference of a non-array" }
  end associate
contains
  function fcn () result (y)
    type(r_t) :: y
    y = make ()
  end function
  function fcn2 () result (y)
    type(r2_t) :: y
  end function
end program
