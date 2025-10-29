! { dg-do compile }
!
! Test fix for PR122434
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module neuron_m
  implicit none

  type neuron_t
    real, allocatable :: weight_
  end type

  interface
    type(neuron_t) pure module function from_json() result(neuron)
    end function
  end interface

contains
  module procedure from_json
    associate(num_inputs => 1)
! Gave "Error: Bad allocate-object at (1) for a PURE procedure" in next line.
      allocate(neuron%weight_, source=0.)
    end associate
  end procedure
end module
