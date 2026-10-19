! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
! { dg-shouldfail "END TEAM without STAT= in a team with a stopped image" }
!
! END TEAM without STAT= in a team with a stopped image is error
! termination.

program end_team_stat_2
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: t

  if (num_images () < 2) error stop
  form team (1, t)
  change team (t)
    if (this_image () == num_images ()) stop
  end team
end program end_team_stat_2
