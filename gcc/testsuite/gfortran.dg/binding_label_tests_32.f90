! { dg-do run }
! PR 77746 - this used to crash during execution.
! Original test case by Vladimir Fuka.
module first
  private
  public execute
  
  interface execute
    module procedure random_name
  end interface
  
contains

  subroutine random_name()
  end subroutine
end module

module test
  use first

  implicit none

contains

  subroutine p_execute(i)  bind(C, name="random_name")
    integer :: i

    call execute()
  end subroutine
  
end module

  use test
  call p_execute(1)
end
