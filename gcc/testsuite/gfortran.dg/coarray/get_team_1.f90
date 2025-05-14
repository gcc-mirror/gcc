!{ dg-do compile }

! PR 97210
! Tests get_team syntax

  use iso_fortran_env
  implicit none
  type(team_type) :: team, ret
  integer :: level

  ret = get_team()
  ret = get_team('abc') !{ dg-error "must be INTEGER" }
  ret = get_team(level, 'abc') !{ dg-error "Too many arguments" }
  ret = get_team([1,2]) !{ dg-error "must be a scalar" }
  ret = get_team(team) !{ dg-error "must be INTEGER" }
  
  ret = get_team(INITIAL_TEAM)
  ret = get_team(CURRENT_TEAM)
  ret = get_team(PARENT_TEAM)
  ret = get_team(INITIAL_TEAM, CURRENT_TEAM) !{ dg-error "Too many arguments" }

  level = INITIAL_TEAM
  ret = get_team(level)
  ret = get_team(99) !{ dg-error "specify one of the INITIAL_TEAM, PARENT_TEAM" }
  level = 99
  ret = get_team(level)
  level = get_team() !{ dg-error "Cannot convert TYPE\\(team_type\\)" }
end

