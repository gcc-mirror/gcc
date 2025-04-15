!{ dg-do run }

! Check coindexes with team= or team_number= are working.

program coindexed_5
  use, intrinsic :: iso_fortran_env

  type(team_type) :: parentteam, team, formed_team
  integer :: t_num= 42, stat = 42, lhs
  integer(kind=2) :: st_num=42
  integer :: caf(2)[*]

  parentteam = get_team()

  caf = [23, 32]
  form team(t_num, team, new_index=1)
  form team(t_num, formed_team)

  change team(team, cell[*] => caf(2))
    ! for get_from_remote
    ! Checking against caf_single is very limitted.
    if (cell[1, team_number=t_num] /= 32) stop 1
    if (cell[1, team_number=st_num] /= 32) stop 2
    if (cell[1, team=parentteam] /= 32) stop 3

    ! Check that team_number is validated
    lhs = cell[1, team_number=5, stat=stat]
    if (stat /= 1) stop 4

    ! Check that only access to active teams is valid
    stat = 42
    lhs = cell[1, team=formed_team, stat=stat]
    if (stat /= 1) stop 5

    ! for send_to_remote
    ! Checking against caf_single is very limitted.
    cell[1, team_number=t_num] = 45
    if (cell /= 45) stop 11
    cell[1, team_number=st_num] = 46
    if (cell /= 46) stop 12
    cell[1, team=parentteam] = 47
    if (cell /= 47) stop 13

    ! Check that team_number is validated
    stat = -1
    cell[1, team_number=5, stat=stat] = 0
    if (stat /= 1) stop 14

    ! Check that only access to active teams is valid
    stat = 42
    cell[1, team=formed_team, stat=stat] = -1
    if (stat /= 1) stop 15

    ! for transfer_between_remotes
    ! Checking against caf_single is very limitted.
    cell[1, team_number=t_num] = caf(1)[1, team_number=-1]
    if (cell /= 23) stop 21
    cell[1, team_number=st_num] = caf(2)[1, team_number=-1]
    ! cell is an alias for caf(2) and has been overwritten by caf(1)!
    if (cell /= 23) stop 22
    cell[1, team=parentteam] = caf(1)[1, team= team]
    if (cell /= 23) stop 23

    ! Check that team_number is validated
    stat = -1
    cell[1, team_number=5, stat=stat] = caf(1)[1, team_number= -1]
    if (stat /= 1) stop 24
    stat = -1
    cell[1, team_number=t_num] = caf(1)[1, team_number= -2, stat=stat]
    if (stat /= 1) stop 25

    ! Check that only access to active teams is valid
    stat = 42
    cell[1, team=formed_team, stat=stat] = caf(1)[1]
    if (stat /= 1) stop 26
    stat = 42
    cell[1] = caf(1)[1, team=formed_team, stat=stat]
    if (stat /= 1) stop 27
  end team
end program coindexed_5
