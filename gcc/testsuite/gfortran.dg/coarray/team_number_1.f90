! { dg-do run }
!
! PR fortran/126777
!
! TEAM_NUMBER() with no argument passes a null team handle, meaning the
! current team.  libcaf_single dereferenced it and crashed.

program team_number_1
  use iso_fortran_env, only : team_type
  implicit none

  type(team_type) :: t

  if (team_number () /= -1) stop 1

  t = get_team ()
  if (team_number (t) /= -1) stop 2
  if (team_number (get_team ()) /= -1) stop 3
end program team_number_1
