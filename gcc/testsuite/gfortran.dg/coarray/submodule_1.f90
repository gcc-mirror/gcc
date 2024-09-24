!{ dg-do run }
!{ dg-additional-sources add_sources/submodule_1_sub.f90 }

! Separating the module and the submodule is needed to show the error.
! Having all code pieces in one file does not show the error.

module pr80235
  implicit none

  private
  public :: test, var

  type T
    integer :: v
  end type T

interface

  module subroutine test()
  end subroutine

end interface

  type(T) :: var[*]

end module pr80235


  
