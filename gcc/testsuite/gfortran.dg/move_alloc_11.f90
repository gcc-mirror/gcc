!{ dg-do compile }

! General error checking for move_alloc parameter list.

integer, allocatable :: i, o
integer :: st, s2
character(30) :: e, e2

  call move_alloc(i, o, STAT=st)
  call move_alloc(i, o, STAT=st, STAT=s2) !{ dg-error "Keyword 'stat' at \\(1\\) has already appeared in the current argument list" }
  call move_alloc(i, o, STAT=e) !{ dg-error "STAT= argument at \\(1\\) must be a scalar INTEGER variable of at least kind 2" }
  call move_alloc(i, o, STAT=[st, s2]) !{ dg-error "STAT= argument at \\(1\\) must be a scalar INTEGER variable of at least kind 2" }
  call move_alloc(i, o, STAT=.TRUE.) !{ dg-error "STAT= argument at \\(1\\) must be a scalar INTEGER variable of at least kind 2" }

  call move_alloc(i, o, STAT=st, ERRMSG=e)
  call move_alloc(i, o, ERRMSG=e) 
  call move_alloc(i, o, ERRMSG=e, ERRMSG=e2) !{ dg-error "Keyword 'errmsg' at \\(1\\) has already appeared in the current argument list" }
  call move_alloc(i, o, ERRMSG=st) !{ dg-error "ERRMSG= argument at \\(1\\) must be a scalar CHARACTER variable of at least kind 1" }
  call move_alloc(i, o, ERRMSG=.TRUE.) !{ dg-error "ERRMSG= argument at \\(1\\) must be a scalar CHARACTER variable of at least kind 1" }
  

end

