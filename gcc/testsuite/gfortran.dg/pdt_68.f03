! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Check the fix for PR122566.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module double_precision_file_m
  implicit none

  type file_t
    integer :: i
  end type

  type, extends(file_t) :: double_precision_file_t
  end type

  type, extends(double_precision_file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
  end type

contains
  pure module function training_configuration()
    type(training_configuration_t) training_configuration
    training_configuration%file_t = file_t(42) ! Needed parent type to be introduced explicitly
  end function
end module

  use double_precision_file_m
  type(training_configuration_t) :: x
  x = training_configuration ()
  if (x%i /= 42) stop 1
end
! { dg-final { scan-tree-dump-times "double_precision_file_t.file_t" 2 "original" } }
