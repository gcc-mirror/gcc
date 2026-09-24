! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
! { dg-shouldfail "an image is killed by a signal" }
!
! An image killed by a signal while its team is the current one must not
! block the remaining images of that team.

program signal_team_1
  use iso_fortran_env, only : team_type, stat_failed_image
  implicit none
  type(team_type) :: t
  integer :: i, st

  form team (merge (1, 2, mod (this_image (), 2) == 1), t)
  change team (t)
    sync all
    if (this_image () == num_images ()) call abort ()

    do i = 1, 100
      st = 0
      sync all (stat=st)
      if (st == stat_failed_image) exit
    end do
    if (num_images () > 1 .and. st /= stat_failed_image) error stop "not reported"
  end team
end program signal_team_1
