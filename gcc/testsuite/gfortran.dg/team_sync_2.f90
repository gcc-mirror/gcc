!{ dg-do run }
!{ dg-additional-options "-fcoarray=lib -fdump-tree-original -lcaf_single" }
!{ dg-additional-options "-latomic" { target libatomic_available } }

! PR 87939
! Test sync team statement
!
  use iso_fortran_env, only : team_type
  implicit none
  integer :: istat = 42
  type(team_type) :: team
  character(len=30) :: err = "unchanged"

  form team (mod(this_image(),2)+1, team)

  change team (team)
    sync team (team)
    sync team (team, stat=istat)
    if (istat /= 0) stop 1
    sync team (team, stat=istat, errmsg=err)
    if (trim(err) /= 'unchanged') stop 2
  end team
end

! { dg-final { scan-tree-dump "_gfortran_caf_sync_team \\(team, 0B, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_sync_team \\(team, &istat, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_sync_team \\(team, &istat, &err, 30\\)" "original" } }
