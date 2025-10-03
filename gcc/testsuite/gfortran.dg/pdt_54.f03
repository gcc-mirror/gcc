! { dg-do compile }
!
! Test the fix for PR122089 in which an error occured in compiling the module
! because a spurious REAL(KIND=0) was being produced for 'values_'.
!
! This is a variant of pdt_52.f03. See the comments in that test.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_
  end type

  type input_output_pair_t(k)
    integer, kind :: k
    type(tensor_t(k)) inputs_     ! Used to ICE if 2nd component dropped
  end type

  type mini_batch_t(k)
    integer, kind :: k
    type(input_output_pair_t(k)) input_output_pairs_
  end type

end module tensor_m
