! { dg-do compile }
!
! Check the fix for PR122524.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_map_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_(:)
  end type

  interface tensor_t
    module function tensor(values)
      implicit none
      double precision values(:)
      type(tensor_t(kind(0D0))) tensor
    end function
  end interface

  type tensor_map_t(k)
    integer, kind :: k = kind(1.)
    real(k) slope_
  end type

contains
  function unnormalized_tensor(self, tensor)
    type(tensor_map_t(kind(0D0))) self
    type(tensor_t(kind(0D0))) tensor, unnormalized_tensor
    associate(unnormalized_values => tensor%values_*self%slope_)
      unnormalized_tensor = tensor_t(unnormalized_values)   ! Caused an ICE.
    end associate
  end function
end module
