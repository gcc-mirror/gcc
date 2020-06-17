! { dg-do compile }
! PR 87644 - this used to cause an ICE.
! Test case by Matt Thompson.
module test

  implicit none
  private
  public :: get
  
contains

  subroutine initialize()
     integer :: parameters
     parameters = get()
  end subroutine initialize

  function get() result(parameters)
     integer :: parameters
     parameters = 1
  end function get

end module test
