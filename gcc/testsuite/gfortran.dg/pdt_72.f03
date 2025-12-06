! { dg-do compile }
!
! Tests the fix for pr122578, which failed in compilation with the errors
! shown below.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_map_m
  use iso_c_binding, only :  c_int
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_(:) ! Error: Cannot convert REAL(0) to REAL(4) at (1)
  contains
    generic   :: values => default_real_values
    procedure default_real_values
  end type

  interface
    pure module function default_real_values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
    end function
  end interface

  type tensor_map_t(k)
    integer, kind :: k = kind(1.)
    real(k), dimension(:), allocatable :: intercept_, slope_
  contains
    generic :: map_to_training_range    => default_real_map_to_training_range
    procedure :: default_real_map_to_training_range
    generic :: map_from_training_range  => default_real_map_from_training_range
    procedure :: default_real_map_from_training_range
  end type

  interface
    elemental module function default_real_map_to_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(tensor_map_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function default_real_map_from_training_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(tensor_map_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) unnormalized_tensor
    end function
  end interface

  type activation_t
    integer(c_int) :: selection_
  contains
    generic :: evaluate => default_real_evaluate
    procedure default_real_evaluate
  end type

  interface
    elemental module function default_real_evaluate(self, x) result(y)
      implicit none
      class(activation_t), intent(in) :: self
      real, intent(in) :: x 
      real y 
    end function
  end interface

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    type(tensor_map_t(k)) input_map_, output_map_
    real(k), allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
    type(activation_t) :: activation_
  contains
    generic :: infer => default_real_infer
    procedure default_real_infer
  end type

  integer, parameter :: input_layer = 0 
contains
  elemental function default_real_infer(self, inputs) result(outputs)
    class(neural_network_t), intent(in) :: self
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) outputs
    real, allocatable :: a(:,:)
    integer l
    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))
      allocate(a(maxval(n), input_layer:output_layer))
      associate(normalized_inputs => self%input_map_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values() ! Error: Symbol ‘normalized_inputs’
                                                                     ! at (1) has no IMPLICIT type

      end associate
      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_%evaluate(z)
        end associate
      end do feed_forward
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
        outputs = self%output_map_%map_from_training_range(normalized_outputs) ! Error: Found no matching specific
                                                                               ! binding for the call to the GENERIC
                                                                               ! ‘map_from_training_range’ at (1)

      end associate
    end associate
  end function
end module
