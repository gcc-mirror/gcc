!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib -fdump-tree-original" }

! PR 87939
! Tests get_team

  use iso_fortran_env
  implicit none
  type(team_type) :: team, ret
  integer :: new_team, level

  new_team = mod(this_image(),2)+1

  form team (new_team,team)

  ret = get_team()
  ret = get_team(INITIAL_TEAM)
  ret = get_team(PARENT_TEAM)
  ret = get_team(CURRENT_TEAM)
  level = INITIAL_TEAM
  ret = get_team(level)
  
end

! { dg-final { scan-tree-dump "_gfortran_caf_get_team \\(0B\\)" "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_get_team \\(&C\.\[0-9\]+\\)" 3 "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_get_team \\(&level\\)" "original" } }
