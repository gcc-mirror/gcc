! { dg-do compile }
!
! Test the fix for PR122089 in which an error occured in compiling the module
! because a spurious REAL(KIND=0) was being produced for 'values_'.
!
! Other failures are indicated by the comments. For reasons that are not to me,
! they didn't fail when combined with this test.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_               ! ICE if not allocatable
  end type

  type input_output_pair_t(k)
    integer, kind :: k
    type(tensor_t(k)) inputs_, expected_outputs_  ! ICE if 2nd component dropped
  end type

  type mini_batch_t(k)
    integer, kind :: k
    type(input_output_pair_t(k)) input_output_pairs_
  end type

end module tensor_m

  use tensor_m
  type (mini_batch_t(k = kind(1d0))) :: x
  allocate (x%input_output_pairs_%inputs_%values_, source = 42d0)
  print *, kind (x%input_output_pairs_%inputs_%values_), x%input_output_pairs_%inputs_%values_
  deallocate (x%input_output_pairs_%inputs_%values_)
end
