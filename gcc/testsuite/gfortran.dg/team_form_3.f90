!{ dg-do run }
!{ dg-additional-options "-fcoarray=lib -fdump-tree-original -lcaf_single" }
!{ dg-additional-options "-latomic" { target libatomic_available } }

! PR 87939
! Tests form team with stat= and errmsg=

  use iso_fortran_env, only : team_type
  implicit none
  integer :: istat = 42, new_team
  character(len=30) :: err = "unchanged"
  type(team_type) :: team

  new_team = mod(this_image(),2)+1

  form team (new_team,team)
  form team (new_team,team,stat=istat)
  if (istat /= 0) stop 1
  form team (new_team,team,stat=istat, errmsg=err)
  if (trim(err) /= 'unchanged') stop 2
  form team (new_team,team,new_index=1)
  istat = 42
  form team (new_team,team,new_index=1,stat=istat)
  if (istat /= 0) stop 3
  form team (new_team,team,new_index=1,stat=istat,errmsg=err)
  if (trim(err) /= 'unchanged') stop 4
end

! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, 0B, 0B, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, 0B, &istat, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, 0B, &istat, &err, 30\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, &C\\.\[0-9\]+, 0B, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, &C\\.\[0-9\]+, &istat, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_form_team \\(new_team, &team, &C\\.\[0-9\]+, &istat, &err, 30\\)" "original" } }
