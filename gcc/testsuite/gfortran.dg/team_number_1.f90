! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Tests if team_number intrinsic fucntion works
!
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) team
  integer, parameter :: standard_initial_value=-1
  integer new_team

  if (team_number()/=standard_initial_value) STOP 1

  new_team = mod(this_image(),2)+1
  form team (new_team,team)
    change team (team)
    if (team_number()/=new_team) STOP 2
  end team

  if (team_number()/=standard_initial_value) STOP 3

end
