! { dg-do compile }
!
! PR122949 used to fail at line 40
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensors_m
  implicit none

  type scalar_t
  contains
    generic :: operator(.grad.) => grad
    procedure grad
  end type

  type vector_t
  contains
    procedure grid
  end type

contains
  function grad(self) result(gradient)
    class(scalar_t), intent(in) :: self
    type(vector_t) gradient
    gradient = vector_t()
  end function

  function grid(self) result(x)
    class(vector_t) self
    real x
    x = 42.0
  end function
end module

  use tensors_m
  implicit none
  type(scalar_t) :: s = scalar_t()

  associate(grad_s => .grad. s)
    associate(grad_s_grid => grad_s%grid()) ! "Error: Invalid association target at (1)"
      if (int (grad_s_grid) /= 42) stop 1
    end associate
  end associate
end
