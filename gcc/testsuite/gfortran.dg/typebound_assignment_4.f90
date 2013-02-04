! { dg-do compile }
!
! PR fortran/54195
! The compiler used to diagnose a duplicate entity in the assignment interface
! because NC was resolved twice.
!
! Contributed by Andrew Benson <abenson@obs.carnegiescience.edu>

module gn

  implicit none

  type :: nc
   contains
     procedure :: assign => nca
     generic   :: assignment(=) => assign
  end type

  type, extends(nc) :: ncb
   contains
     procedure , nopass :: tis => bf
  end type

contains

  subroutine nca(to,from)
    class(nc), intent(out) :: to
    type(nc), intent(in) :: from
  end subroutine

  logical function bf()
    bf=.false.
  end function

end module
