! { dg-do compile }
!
! PR 58023: [F03] ICE on invalid with bad PPC declaration
!
! Contributed by Andrew Benson <abensonca@gmail.com>

  implicit none

  type :: sfd
    procedure(mr), pointer :: mr2  ! { dg-error "must be explicit" }
  end type

  type(sfd):: d
  print *, d%mr2()

end
