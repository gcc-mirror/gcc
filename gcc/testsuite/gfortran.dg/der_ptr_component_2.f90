! { dg-do run }
!
! Freeing the width_data lead to double free. This testcase tests that
! pr79230 is fixed now.

program main_ut
  implicit none

  type :: data_t
     character, allocatable :: c1
  end type

  type :: t1_t
     character, allocatable :: c2
     class(data_t), pointer :: width_data
  end type

  call evaluator

contains

  subroutine evaluator
    type(data_t), target :: par_real
    type(t1_t) :: field
    field%width_data => par_real
  end subroutine

end


