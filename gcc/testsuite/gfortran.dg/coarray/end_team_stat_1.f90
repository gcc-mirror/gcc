! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
!
! END TEAM (STAT=) reports a stopped or failed image of the team it ends,
! not one of a sibling team, and still deallocates the team's coarrays.

program end_team_stat_1
  use iso_fortran_env, only : team_type, stat_stopped_image, stat_failed_image
  implicit none
  type(team_type) :: t
  integer, allocatable :: a[:]
  integer :: me, st
  character(len=40) :: msg

  if (num_images () < 6) stop
  me = mod (this_image () - 1, 3) + 1
  form team (me, t)

  change team (t)
    msg = "unchanged"
  end team (stat=st, errmsg=msg)
  if (st /= 0 .or. msg /= "unchanged") error stop 1

  change team (t)
    allocate (a[*])
    if (this_image () == num_images ()) then
      if (me == 1) stop
      if (me == 2) fail image
    end if
    st = -1
    msg = "unchanged"
  end team (stat=st, errmsg=msg)
  if (allocated (a)) error stop 2
  select case (me)
  case (1)
    if (st /= stat_stopped_image .or. msg == "unchanged") error stop 3
  case (2)
    if (st /= stat_failed_image .or. msg == "unchanged") error stop 4
  case (3)
    if (st /= 0 .or. msg /= "unchanged") error stop 5
  end select
end program end_team_stat_1
