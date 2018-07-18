! { dg-do run }
! { dg-options "-Wall" }
! Added -Wall option to make sure PR42526 does not show up again.
program gfcbug62
  implicit none
  character(len=16) ::    tdefi(2) = (/'0z1jan0000','1hr       '/)
  type t_ctl
     character(len=16) :: tdefi(2) = (/'0z1jan0000','1hr       '/)
  end type t_ctl

  type(t_ctl) :: ctl
  integer     :: i,k
  i = 1
  k = 1
  if (tdefi(1) .ne. ctl%tdefi(1)) STOP 1
end program gfcbug62
