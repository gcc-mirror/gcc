!{ dg-do run }

program main
  use, intrinsic :: iso_fortran_env, only: team_type
  implicit none
  integer, parameter :: PARENT_TEAM = 1, CURRENT_TEAM = 2, CHILD_TEAM = 3
  type(team_type) :: team(3)

  if (num_images() > 7) then

    form team (1, team(PARENT_TEAM))
    change team (team(PARENT_TEAM))
      form team (mod(this_image(),2) + 1, team(CURRENT_TEAM))
      change team (team(CURRENT_TEAM))
        form team(mod(this_image(),2) + 1, team(CHILD_TEAM))
        sync team(team(PARENT_TEAM))
        ! change order / number of syncs between teams to try to expose deadlocks
        if (team_number() == 1) then
           sync team(team(CURRENT_TEAM))
           sync team(team(CHILD_TEAM))
        else
           sync team(team(CHILD_TEAM))
           sync team(team(CURRENT_TEAM))
           sync team(team(CHILD_TEAM))
           sync team(team(CURRENT_TEAM))
        end if
      end team
    end team

    sync all
  end if

end program
