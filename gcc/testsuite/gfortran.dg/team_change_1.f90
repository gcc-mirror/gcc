! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Tests if change team worked
!
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) team
  integer new_team

  new_team = mod(this_image(),2)+1

  form team (new_team,team)
    change team (team)
    if (team_number()/=new_team) STOP 1
  end team

end
