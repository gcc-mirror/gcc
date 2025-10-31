! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test fix for PR122452
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module kind_parameters_m
  integer, parameter :: default_real = kind(1e0)
  integer, parameter :: double_precision = kind(1d0)
end module

module tensor_m
  use kind_parameters_m, only : default_real, double_precision
  implicit none

  private
  public :: tensor_t

  type tensor_t(k)
    integer, kind :: k = default_real 
    real(k), allocatable, private :: values_(:)
  contains
    generic   :: values => default_real_values, double_precision_values
    procedure, private, non_overridable ::  default_real_values, double_precision_values
    generic :: num_components => default_real_num_components, double_precision_num_components
    procedure, private ::        default_real_num_components, double_precision_num_components
  end type

  interface tensor_t

    pure module function construct_default_real(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function

    pure module function construct_double_precision(values) result(tensor)
      implicit none
      double precision, intent(in) :: values(:)
      type(tensor_t(double_precision)) tensor
    end function

  end interface

  interface

    pure module function default_real_values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
    end function

    pure module function double_precision_values(self) result(tensor_values)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      double precision, allocatable :: tensor_values(:)
    end function

    pure module function default_real_num_components(self) result(n)
      implicit none
      class(tensor_t), intent(in) :: self
      integer n
    end function

    pure module function double_precision_num_components(self) result(n)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      integer n
    end function

  end interface

end module tensor_m

submodule(tensor_m) tensor_s
contains

    pure module function construct_default_real(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
      tensor = tensor_t ()(values)
    end function

    pure module function construct_double_precision(values) result(tensor)
      implicit none
      double precision, intent(in) :: values(:)
      type(tensor_t(double_precision)) tensor
      tensor = tensor_t (double_precision)(values)
    end function

    pure module function default_real_values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
      tensor_values = self%values_
    end function

    pure module function double_precision_values(self) result(tensor_values)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      double precision, allocatable :: tensor_values(:)
      tensor_values = self%values_
    end function


    pure module function default_real_num_components(self) result(n)
      implicit none
      class(tensor_t), intent(in) :: self
      integer n
      n = default_real
    end function

    pure module function double_precision_num_components(self) result(n)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      integer n
      n = double_precision
    end function

end submodule tensor_s


  use tensor_m
  type(tensor_t(kind(0e0))) :: a
  type(tensor_t(kind(0D0))) :: b
  a = tensor_t ([1e0,2e0])
  print *, a%num_components (), a%values ()
  b = tensor_t ([3d0,4d0])
  print *, b%num_components (), b%values ()
end
! { dg-final { scan-tree-dump-times "construct_" 4 "original" } }
! { dg-final { scan-tree-dump-times "_components" 4 "original" } }
! { dg-final { scan-tree-dump-times "_values" 4 "original" } }
