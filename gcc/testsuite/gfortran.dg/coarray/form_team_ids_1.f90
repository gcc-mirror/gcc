! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
! PR 127738
!
! Each team formed gets its own shared data: team 2 and then team 1 formed
! from the same team, and a team formed after only some of its images formed
! a team inside a child team.

program form_team_ids_1
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: t1, t2, inner, t3
  integer :: me, n

  me = this_image ()
  n = num_images ()
  form team (2, t1)
  form team (1, t2, new_index = n + 1 - me)
  change team (t2)
    if (team_number () /= 1 .or. num_images () /= n) error stop 1
    if (this_image () /= n + 1 - me) error stop 2
  end team

  form team (mod (me - 1, 2) + 1, t1)
  change team (t1)
    if (team_number () == 1) form team (1, inner)
  end team
  form team (1, t3)
  change team (t3)
    if (num_images () /= n) error stop 3
  end team
end program form_team_ids_1
