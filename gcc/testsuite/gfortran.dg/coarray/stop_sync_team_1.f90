! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
!
! An image stopping in one team is a stopped image for SYNC ALL and SYNC
! TEAM in that team only, not in its sibling team.

program stop_sync_team_1
  use iso_fortran_env, only : team_type, stat_stopped_image
  implicit none
  type(team_type) :: t
  integer :: i, st, me

  if (num_images () < 4) stop
  me = 2 - mod (this_image (), 2)
  form team (me, t)
  change team (t)
    sync all
    if (me == 1 .and. this_image () == num_images ()) stop

    do i = 1, 5
      st = -1
      sync all (stat=st)
      if (me == 2 .and. st /= 0) error stop 1
    end do
    if (me == 1 .and. st /= stat_stopped_image) error stop 2

    st = -1
    sync team (get_team (), stat=st)
    if (me == 2 .and. st /= 0) error stop 3
    if (me == 1 .and. st /= stat_stopped_image) error stop 4
  end team
end program stop_sync_team_1
