! { dg-do run }
! PR fortran/126968
! ICE in gfc_conv_component_ref when the type of an associate name, whose
! selector is a function that has not yet been parsed, is guessed wrongly.
! The guess is the first accessible type with a component of the right name,
! so 'wrong_t' is picked and has to be corrected during resolution.
!
module m
  type :: wrong_t
     integer :: x = 1
     integer :: pad = 0
  end type
  type :: right_t
     real :: junk = 0.0
     real :: x = 2.5
  end type
  type :: outer_t
     type(right_t) :: c
  end type
end module

program p
  use m
  implicit none
  real :: r, ra(3)

  associate (q => scalar_fcn ())
    r = q%x
  end associate
  if (abs (r - 2.5) > 1.0e-6) stop 1

  associate (q => array_fcn ())
    ra = q(:)%x
  end associate
  if (any (abs (ra - 2.5) > 1.0e-6)) stop 2

  associate (q => nested_fcn ())
    r = q%c%x
  end associate
  if (abs (r - 2.5) > 1.0e-6) stop 3

contains

  function scalar_fcn () result (y)
    type(right_t) :: y
  end function

  function array_fcn () result (y)
    type(right_t) :: y(3)
  end function

  function nested_fcn () result (y)
    type(outer_t) :: y
  end function

end program
