! { dg-do compile }
!
! Test the fix for PR123952, which failed as below.
!
! Contributed by Damian.Rouson  <damian@archaeologic.codes>
!
module tensors_1D_m
  abstract interface
     function scalar_1D_initializer_i() result(f)
      double precision, allocatable :: f(:)
    end function
  end interface

  type :: scalar_1D_t
    integer gradient_operator_1D_
  end type

  interface scalar_1D_t
     module function construct_1D_scalar_from_function(initializer) result(scalar_1D)
      procedure(scalar_1D_initializer_i), pointer :: initializer
      type(scalar_1D_t) scalar_1D
    end function
  end interface

end module tensors_1D_m

submodule(tensors_1D_m) scalar_1D_s
contains

  module procedure construct_1D_scalar_from_function ! "MODULE PROCEDURE at (1) must be
                                                     ! in a generic module interface"
      scalar_1D = scalar_1D_t (42)                   ! "Unexpected assignment statement..."
  end procedure                                      ! "Expecting END SUBMODULE statement at (1)"

end submodule scalar_1D_s
