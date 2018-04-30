! { dg-do  run }
! PR 59781 - this was not initialized correctly before.
! Original test case by James Spencer.
  implicit none
  
  type t1
    integer :: s
  end type
  
  type t2
    type(t1) :: state = t1(1)
    real, allocatable :: store(:)
  end type
  
  call test

contains

  subroutine test
    type(t2) :: rng
    if (rng%state%s /= 1) STOP 1
  end subroutine

end
