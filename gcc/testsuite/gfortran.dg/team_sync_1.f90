!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib" }

! PR 87939
! Test sync team syntax errors

  use iso_fortran_env, only : team_type
  implicit none
  integer :: istat
  character(len=30) :: err
  type(team_type) :: team

  form team (mod(this_image(),2)+1, team)

  change team (team)
    sync team ! { dg-error "Syntax error in SYNC TEAM statement" }
    sync team (err) ! { dg-error "must be a scalar expression of type TEAM_TYPE" }
    sync team (team, istat) ! { dg-error "Syntax error in SYNC TEAM statement" }
    sync team (team, stat=err) ! { dg-error "must be a scalar INTEGER" }
    sync team (team, stat=istat, stat=istat) ! { dg-error "Duplicate STAT" }
    sync team (team, stat=istat, errmsg=istat) ! { dg-error "must be a scalar CHARACTER variable" }
    sync team (team, stat=istat, errmsg=err, errmsg=err) ! { dg-error "Duplicate ERRMSG" }
  end team
end
