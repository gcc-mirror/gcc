!{ dg-do compile }

program coindexed_2
  use, intrinsic :: iso_fortran_env 

  integer, save :: dim1[*]
  integer :: ist
  logical :: cst
  type(team_type) :: team

  dim1 = 3
  print *, dim1[1] ! ok
  print *, dim1['me'] ! { dg-error "Array index at \\\(1\\\) must be of INTEGER" }

  print *, dim1[1, STAT=ist] !ok
  print *, dim1[1, STAT=cst] ! { dg-error "STAT argument at \\\(1\\\) must be of INTEGER" }
  print *, dim1[1, STAT=[ist]] ! { dg-error "STAT argument at \\\(1\\\) must be scalar" }
  print *, dim1[1, STAT=ist, STAT=ist]  ! { dg-error "Duplicate" }
  print *, dim1[STAT=ist, 1] ! { dg-error "Invalid form of" }
  print *, dim1[5, STAT=ist, 1] ! { dg-error "Invalid form of" }
  print *, dim1[5, STAT=dim1[1]] ! { dg-error "Expression in STAT= at \\\(1\\\) must not be coindexed" }

  print *, dim1[1, TEAM=team] !ok
  print *, dim1[1, STAT= ist, TEAM=team] !ok
  print *, dim1[1, TEAM=team, STAT=ist] !ok
  print *, dim1[1, STAT=ist, TEAM=team, STAT=ist] ! { dg-error "Duplicate" }
  print *, dim1[1, TEAM=team, STAT=ist, TEAM=team] ! { dg-error "Duplicate" }
  print *, dim1[1, TEAM=ist] ! { dg-error "TEAM argument at \\\(1\\\) must be of TEAM_TYPE" }
  print *, dim1[1, TEAM=[team]] ! { dg-error "TEAM argument at \\\(1\\\) must be scalar" }
  print *, dim1[TEAM=team, 1] ! { dg-error "Invalid form of" }
  print *, dim1[5, TEAM=team, 1] ! { dg-error "Invalid form of" }

  print *, dim1[1, TEAM_NUMBER=-1] !ok
  print *, dim1[1, TEAM_NUMBER=1] !ok
  print *, dim1[1, TEAM_NUMBER=1.23] ! { dg-error "TEAM_NUMBER argument at \\\(1\\\) must be of INTEGER" }
  print *, dim1[1, TEAM_NUMBER='me'] ! { dg-error "TEAM_NUMBER argument at \\\(1\\\) must be of INTEGER" }
  print *, dim1[1, TEAM_NUMBER=5, STAT=ist] !ok
  print *, dim1[1, TEAM_NUMBER=5, STAT=ist, TEAM_NUMBER=-1] ! { dg-error "Duplicate" }
  print *, dim1[1, TEAM_NUMBER=-1, TEAM=team] ! { dg-error "Only one of TEAM" }
  print *, dim1[TEAM_NUMBER=-1, 1] ! { dg-error "Invalid form of" }
  print *, dim1[5, TEAM_NUMBER=-1, 1] ! { dg-error "Invalid form of" }
end program


