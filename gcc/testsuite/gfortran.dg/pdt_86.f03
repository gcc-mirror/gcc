! { dg-do run }
!
! Test the fix for PR122902. Line 47 gave "free(): invalid pointer".
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module input_output_pair_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_(:)
  end type

  type input_output_pair_t(k)
    integer, kind :: k = kind(1.)
    type(tensor_t(k)) inputs_, expected_outputs_
  end type

  interface
    type(input_output_pair_t) elemental module function input_output_pair(inputs, expected_outputs)
      implicit none
      type(tensor_t), intent(in) :: inputs, expected_outputs
    end function
  end interface

end module

submodule(input_output_pair_m) input_output_pair_s
  implicit none
contains
  module procedure input_output_pair
    input_output_pair%inputs_ = inputs
    input_output_pair%expected_outputs_ = expected_outputs
  end procedure
end submodule

  use input_output_pair_m
  implicit none
  type(tensor_t), allocatable :: inputs(:), outputs(:)
  type(input_output_pair_t), allocatable :: input_output_pairs(:), mini_batch(:)
  integer i

  inputs = [(tensor_t([real(i)]), i=1,7)]
  outputs = inputs
  input_output_pairs = input_output_pair(inputs, outputs)
  mini_batch = input_output_pairs(1:1)     ! Original failure
  if (any (mini_batch(1)%inputs_%values_ /= 1.0)) stop 1
  mini_batch = input_output_pairs(1:2)     ! Also failed
  if (any (mini_batch(2)%inputs_%values_ /= 2.0)) stop 2
  mini_batch = input_output_pairs          ! Was OK
  if (any (mini_batch(5)%inputs_%values_ /= 5.0)) stop 3
  if (allocated(inputs)) deallocate(inputs)
  if (allocated(outputs)) deallocate(outputs)
  if (allocated(input_output_pairs)) deallocate(input_output_pairs)
  if (allocated(mini_batch)) deallocate(mini_batch)
end
