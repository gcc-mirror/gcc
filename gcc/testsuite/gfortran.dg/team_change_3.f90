!{ dg-do run }
!{ dg-additional-options "-fcoarray=lib -fdump-tree-original -lcaf_single" }
!{ dg-additional-options "-latomic" { target libatomic_available } }

! PR 87939
! Tests change team stat= and errmsg= specifiers

  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team
  integer :: new_team, istat = 42
  character(len=30) :: err = 'unchanged'

  new_team = mod(this_image(),2)+1

  form team (new_team,team)

  change team (team, stat=istat)
    if (istat /= 0) stop 1
  end team

  change team (team, stat=istat, errmsg=err)
    if (trim(err) /= 'unchanged') stop 2
  end team

end

! { dg-final { scan-tree-dump "_gfortran_caf_change_team \\(team, &istat, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_change_team \\(team, &istat, &err, 30\\)" "original" } }
