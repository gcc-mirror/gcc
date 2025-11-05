! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Check the fix for PR122501.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, private :: values_(:)
  contains
    procedure default_real_values
  end type

  interface tensor_t
    type(tensor_t) module function construct_default_real(values)
      implicit none
      real values(:)
    end function
  end interface

  interface
    module function default_real_values(self) result(tensor_values)
      implicit none
      class(tensor_t) self
      real, allocatable :: tensor_values(:)
    end function
  end interface
end module 

  use tensor_m
  implicit none
contains
  function copy(tensor)
    type(tensor_t) tensor, copy, norm_copy
    associate(tensor_values => tensor%default_real_values())

! This gave: "Component ‘values_’ at (1) is a PRIVATE component of ‘tensor_t’"
      copy = tensor_t(tensor_values)

    end associate

! Make sure that the fix really works :-)
    associate(f => tensor%default_real_values())
      associate(tensor_values => tensor%default_real_values())
        norm_copy = tensor_t(tensor_values/maxval(f))
      end associate
    end associate
  end function
end
! { dg-final { scan-tree-dump-times "default_real_values" 3 "original" } }
