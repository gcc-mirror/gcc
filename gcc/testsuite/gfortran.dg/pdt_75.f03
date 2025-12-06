! { dg-do compile }
!
! Tests the fix for pr122693, which failed in compilation with the errors
! shown below.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(0.)
  end type

  interface tensor_t
    module function tensor(unused_stuff)
      implicit none
      real unused_stuff
      type(tensor_t) tensor
    end function
  end interface

end module

  use tensor_m
  implicit none
contains
  function test_passed()
    logical test_passed
    type(tensor_t), allocatable :: tensor_array(:)
    real, parameter :: junk = 0.
    tensor_array = [tensor_t(junk)] !Error: Symbol ‘junk’ at (1) has no IMPLICIT type
    test_passed =  .false.          !Error: ‘test_passed’ at (1) is not a variable
  end function
end
