! { dg-do run }
! Check the fix for PR31487 in which the derived type default initializer
! would be padded out with nulls instead of spaces.
!
! Reported by Harald Anlauf <anlauf@gmx.de>
!
program gfcbug62
  implicit none
  character(len=16) ::    tdefi(2) = (/'0z1jan0000','1hr       '/)
  type t_ctl
     character(len=16) :: tdefi(2) = (/'0z1jan0000','1hr       '/)
  end type t_ctl

  type(t_ctl) :: ctl
  integer     :: i,k

  if (tdefi(1) .ne. ctl%tdefi(1)) STOP 1
end program gfcbug62
