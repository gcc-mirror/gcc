!{ dg-do compile }
!{ dg-additional-options "-fcoarray=lib" }

! Test critical syntax errors with stat= and errmsg= specifiers

  implicit none
  integer :: istat
  character(len=30) :: err
  integer(kind=1) :: too_small_stat

  critical (stat=err) !{ dg-error "must be a scalar INTEGER" }
    continue
  end critical

  critical (stat=istat, stat=istat) !{ dg-error "Duplicate STAT" }
    continue
  end critical !{ dg-error "Expecting END PROGRAM" }

  critical (stat=istat, errmsg=istat) !{ dg-error "must be a scalar CHARACTER variable" }
    continue
  end critical

  critical (stat=istat, errmsg=err, errmsg=err) !{ dg-error "Duplicate ERRMSG" }
    continue
  end critical !{ dg-error "Expecting END PROGRAM" }

  critical (stat=too_small_stat) !{ dg-error "scalar INTEGER variable of at least kind 2" }
    continue
  end critical 
end
