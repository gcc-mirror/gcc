!{ dg-do run }
!{ dg-additional-options "-fcoarray=lib -fdump-tree-original -lcaf_single" }
!{ dg-additional-options "-latomic" { target libatomic_available } }

! PR 87939
! Tests end team stat= and errmsg= specifiers

  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team
  integer :: new_team, istat = 42
  character(len=30) :: err = 'unchanged'
  integer, allocatable :: sample(:)[:]
  integer, allocatable :: scal_caf[:]

  new_team = mod(this_image(),2)+1

  form team (new_team,team)

  change team (team)
    allocate(sample(5)[*], scal_caf[*])
    if (.NOT. allocated(sample)) stop 1
    if (.NOT. allocated(scal_caf)) stop 2
  end team (stat=istat)
  if (istat /= 0) stop 3
  if (allocated(sample)) stop 4
  if (allocated(scal_caf)) stop 5

  deallocate(sample, stat=istat)
  if (istat == 0) stop 6

  istat = 42
  t: change team (team)
    continue
  end team (stat=istat, errmsg=err) t
  if (istat /= 0) stop 7
  if (trim(err) /= 'unchanged') stop 8
end

! { dg-final { scan-tree-dump "_gfortran_caf_end_team \\(&istat, 0B, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_end_team \\(&istat, &err, 30\\)" "original" } }
