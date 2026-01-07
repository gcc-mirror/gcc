! { dg-do compile }
! { dg-options "-Wall -fdump-tree-original" }
!
! Test the fix for PR123071, which caused an ICE.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module neural_network_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    integer :: j = 42
  end type

  type neural_network_t
    integer :: i = 42
  contains
    procedure map_tensor
  end type

  interface
    module function map_tensor(self)
      implicit none
      class(neural_network_t) self
      type(tensor_t) map_tensor
    end function
  end interface
end module

submodule(neural_network_m) neural_network_s
contains
    module procedure map_tensor ! { dg-warning "Return value of function .map_tensor. at .1. not set" }
!      map_tensor%j = 42        ! Uncommenting this makes the warning disappear of course.
    end procedure
end submodule

  use neural_network_m
  implicit none
  type, extends(neural_network_t) ::  trainable_network_t
  end type
  type (trainable_network_t) x
  call foo (x)

contains

  subroutine foo(self)
    class(trainable_network_t) self
    type(tensor_t) mapped_tensor
    mapped_tensor = self%map_tensor()
    if (mapped_tensor%k /= 4) stop 1
    if (mapped_tensor%j /= 42) stop 2
    associate (mt => self%map_tensor())
      if (mt%k /= 4) stop 3
      if (mt%j /= 42) stop 4
    end associate
  end subroutine                ! { dg-warning ".mapped_tensor. is used uninitialized" }

end
! { dg-final { scan-tree-dump-times "Pdttensor_t_4.2.j = 42" 1 "original" } }
! { dg-final { scan-tree-dump-times "struct Pdttensor_t_4 mt" 1 "original" } }
