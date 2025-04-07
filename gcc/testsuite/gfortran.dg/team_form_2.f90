!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib" }

! PR 87939
! Tests form team syntax errors

  use iso_fortran_env, only : team_type
  implicit none
  integer :: istat, new_team
  character(len=30) :: err
  type(team_type) :: team

   new_team = mod(this_image(),2)+1

  form team ! { dg-error "Syntax error in FORM TEAM statement" }
  form team (new_team) ! { dg-error "Syntax error in FORM TEAM statement" }
  form team (new_team,err) ! { dg-error "must be a scalar expression of type TEAM_TYPE" }
  form team (new_team,team,istat) ! { dg-error "Syntax error in FORM TEAM statement" }
  form team (new_team,team,stat=istat,stat=istat) ! { dg-error "Duplicate STAT" }
  form team (new_team,team,stat=istat,errmsg=istat) ! { dg-error "must be a scalar CHARACTER variable" }
  form team (new_team,team,stat=istat,errmsg=err,errmsg=err) ! { dg-error "Duplicate ERRMSG" }
  form team (new_team,team,new_index=1,new_index=1) ! { dg-error "Duplicate NEW_INDEX" }
  form team (new_team,team,new_index=err) ! { dg-error "must be a scalar INTEGER" }
  form team (new_team,team,new_index=1,new_index=1,stat=istat,errmsg=err) ! { dg-error "Duplicate NEW_INDEX" }
  form team (new_team,team,new_index=1,stat=istat,errmsg=err,new_index=9) ! { dg-error "Duplicate NEW_INDEX" }

end
