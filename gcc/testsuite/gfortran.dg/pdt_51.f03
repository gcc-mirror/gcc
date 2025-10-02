! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR122089 in which the generic interface checking failed.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k) values_
  contains
    generic :: values => double_precision_values
    procedure double_precision_values
  end type

contains
  function double_precision_values(self)
    class(tensor_t(kind(1D0))) self
    double precision double_precision_values
    double_precision_values = self%values_
  end function
end module

module input_output_pair_m
  use tensor_m, only : tensor_t
  implicit none

  type input_output_pair_t(k)
    integer, kind :: k = kind(1.)
    type(tensor_t(k)) inputs_
  end type

  interface
    module subroutine double_precision_write_to_stdout(input_output_pairs)
      implicit none
      type(input_output_pair_t(kind(1D0))) input_output_pairs
    end subroutine
  end interface
end module

submodule(input_output_pair_m) input_output_pair_s
  implicit none
contains
  module procedure double_precision_write_to_stdout
    print *, input_output_pairs%inputs_%values()
  end procedure
end submodule

  use input_output_pair_m
  type(input_output_pair_t(kind(1d0))) :: tgt
  tgt%inputs_%values_ = 42d0
  call double_precision_write_to_stdout(tgt)
end
! { dg-final { scan-tree-dump-times "double_precision_write_to_stdout \\(&tgt\\);" 1 "original" } }
