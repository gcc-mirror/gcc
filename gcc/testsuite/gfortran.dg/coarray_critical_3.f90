! { dg-do run }
! { dg-options "-fcoarray=lib -fdump-tree-original -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! PR 87939
! Test critical construct with stat= and errmsg= specifiers
!
  use, intrinsic :: iso_fortran_env, only: int16
  implicit none
  integer :: istat = 42
  integer(kind=int16) :: istat16 = 42
  character(len=30) :: err = 'unchanged'
  integer :: fail = 0

  critical (stat=istat, errmsg=err)
    if (istat /= 0) fail = 1
    if (trim(err) /= 'unchanged') fail = 2
  end critical
  
  if (fail /= 0) stop fail

  critical (stat=istat16, errmsg=err)
    if (istat16 /= 0) fail = 3
    if (trim(err) /= 'unchanged') fail = 4
  end critical

  if (fail /= 0) stop fail
end

! { dg-final { scan-tree-dump "_gfortran_caf_lock \\(caf_token\\.\[0-9\]+, 0, 1, 0B, &istat, &err, 30\\);" "original" } }
! { dg-final { scan-tree-dump "_gfortran_caf_lock \\(caf_token\\.\[0-9\]+, 0, 1, 0B, &stat\\.\[0-9\]+, &err, 30\\);" "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_unlock \\(caf_token\\.\[0-9\]+, 0, 1, &stat\\.\[0-9\]+, 0B, 0\\);" 2 "original" } }
