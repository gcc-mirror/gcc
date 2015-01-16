! { dg-do compile }
!
! PR 58023: [F03] ICE on invalid with bad PPC declaration
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module m
  implicit none

  abstract interface
     double precision function mr()
     end function mr
  end interface

  type :: sfd
     procedure(mr), pointer :: mr1  ! { dg-error "must have at least one argument" }
     procedure(mr), pointer :: mr2  ! { dg-error "must have at least one argument" }
  end type sfd

contains

  subroutine go()
    implicit none
    type(sfd):: d

    write (0,*) d%mr2()
    return
  end subroutine go

end module m

! { dg-final { cleanup-modules "m" } }
