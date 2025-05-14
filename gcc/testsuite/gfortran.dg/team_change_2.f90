!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib" }

! PR 87939
! Tests change team syntax

  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team
  integer :: new_team, istat
  character(len=30) :: err
  integer :: caf[*], caf2[*]

  new_team = mod(this_image(),2)+1

  form team (new_team,team)

  change team !{ dg-error "Syntax error in CHANGE TEAM statement" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  change team (err) !{ dg-error "must be a scalar expression of type TEAM_TYPE" }
    continue
  end team

  change team (team, stat=err) !{ dg-error "must be a scalar INTEGER" }
    continue
  end team

  change team (team, stat=istat, stat=istat) !{ dg-error "Duplicate STAT" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  change team (team, stat=istat, errmsg=istat) !{ dg-error "must be a scalar CHARACTER variable" }
    continue
  end team

  change team (team, stat=istat, errmsg=str, errmsg=str) !{ dg-error "Duplicate ERRMSG" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

1234 if (istat /= 0) stop 1 !{ dg-error "leaves CHANGE TEAM" }

  change team (team)
    go to 1234 !{ dg-error "leaves CHANGE TEAM" }
  end team

  call foo(team)

  ! F2018, C1113
  change team (team, caf[3,*] => caf) !{ dg-error "Codimension decl name" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  change team (team, c[3,*] => caf, c => caf2) !{ dg-error "Duplicate name" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  change team (team, c[3,*] => caf, caf => caf2) !{ dg-error "Codimension decl name" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  change team (team, caf2[3,*] => caf, c => caf2) !{ dg-error "Codimension decl name" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  ! F2018, C1114
  change team (team, c => [caf, caf2]) !{ dg-error "a named coarray" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  ! F2018, C1115
  change team (team, c => caf, c2 => caf) !{ dg-error "duplicates selector at" }
    continue
  end team !{ dg-error "Expecting END PROGRAM statement" }

  t: change team(team)
    exit t
  end team t

  change team(team)
    exit t !{ dg-error "EXIT statement at \\(1\\) is not within construct 't'" }
  end team
contains
  subroutine foo(team)
    type(team_type) :: team

    change team (team)
      return !{ dg-error "Image control statement" }
    end team
  end subroutine
end

