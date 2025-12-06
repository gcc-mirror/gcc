! { dg-do compile }
!
! Tests the fix for pr122670, where use only did not compile for PDTs. Also, it
! was found in the course of developing the fix that import only did not work
! either.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(0.)
    real(k), allocatable :: value_
  end type

  interface
    function myfunc (arg)
      import tensor_t
      implicit none
      type (tensor_t) myfunc
      type (tensor_t), intent(in) :: arg
    end function
  end interface

contains
  function y(x)
    type(tensor_t) x, y
    y = tensor_t(x%value_)
  end function
end module

function myfunc (arg)
  use tensor_m, only : tensor_t
  implicit none
  type (tensor_t) myfunc
  type (tensor_t), intent(in) :: arg
  myfunc = arg
  myfunc%value_ = myfunc%value_ * 2.0
end function

  use tensor_m, only : tensor_t, y, myfunc
  implicit none
  type(tensor_t) desired_output
  desired_output = y(tensor_t(42.))
  desired_output = myfunc (desired_output)
  print *, desired_output%value_
end
