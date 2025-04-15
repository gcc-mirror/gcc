!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib" }

! PR 87939
! Tests change team syntax

  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team
  integer :: new_team, istat
  character(len=30) :: err

  new_team = mod(this_image(),2)+1

  form team (new_team,team)

  change team (team)
    continue
  end team (stat=err) ! { dg-error "must be a scalar INTEGER" }

  change team (team)
    continue
  end team (stat=istat, stat=istat) ! { dg-error "Duplicate STAT" }

  change team (team)
    continue
  end team (stat=istat, errmsg=istat) ! { dg-error "must be a scalar CHARACTER variable" }

  change team (team)
    continue
  end team (stat=istat, errmsg=err, errmsg=err) ! { dg-error "Duplicate ERRMSG" }

  t: change team (team)
    continue
  end team (stat=istat) t ! ok

  t2: change team (team)
    continue
  end team   ! { dg-error "Expected block name of 't2' in END TEAM" }
  end team t2  ! close the team correctly to catch other errors
end

