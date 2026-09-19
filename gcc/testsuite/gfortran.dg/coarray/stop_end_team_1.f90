! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
!
! In a team with a stopped image, SYNC ALL (STAT=) on some images must not
! pair with END TEAM on the others.

program stop_end_team_1
  use iso_fortran_env, only : team_type, stat_stopped_image
  implicit none
  type(team_type) :: t
  integer :: me, st, k

  me = 2 - mod (this_image (), 2)
  form team (me, t)
  change team (t)
    sync all
    if (me == 1 .and. num_images () > 1 .and. this_image () == num_images ()) stop
    if (me == 1 .and. mod (this_image (), 2) == 0) then
      do k = 1, 3
        sync all (stat=st)
        if (st /= 0 .and. st /= stat_stopped_image) error stop 1
      end do
    end if
  end team
  st = -1
  sync all (stat=st)
  if (num_images () > 2 .and. st /= stat_stopped_image) error stop 2
end program stop_end_team_1
