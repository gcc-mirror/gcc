! Test some restrictions on the specifiers of OPEN and CLOSE statements.
! Contributed by Francois-Xavier Coudert (coudert@clipper.ens.fr)
!
! { dg-do compile }
! { dg-options "-ffree-line-length-none -pedantic -fmax-errors=50" }
  integer,parameter :: mone = -1, zero = 0
  character(len=*),parameter :: foo = "foo"
  character(len=20) :: str
  integer :: u

! Test for warnings, when IOSTAT is used

  open(10, iostat=u,access="sequential   ")
  open(10, iostat=u,access="sequential   u") ! { dg-warning "ACCESS specifier in OPEN statement" }
  open(10, iostat=u,access=foo) ! { dg-warning "ACCESS specifier in OPEN statement" }
  open(10, iostat=u,access="direct")
  open(10, iostat=u,access="stream")
  open(10, iostat=u,access="append") ! { dg-warning "Extension: ACCESS specifier in OPEN statement" }

  open(10, iostat=u,action="read")
  open(10, iostat=u,action="write")
  open(10, iostat=u,action="readwrite")
  open(10, iostat=u,action=foo) ! { dg-warning "ACTION specifier in OPEN statement" }

  open(10, iostat=u,blank="ZERO")
  open(10, iostat=u,blank="nUlL")
  open(10, iostat=u,blank="NULLL") ! { dg-warning "BLANK specifier in OPEN statement" }

  open(10, iostat=u,delim="apostrophe")
  open(10, iostat=u,delim="quote")
  open(10, iostat=u,delim="none")
  open(10, iostat=u,delim="") ! { dg-warning "DELIM specifier in OPEN statement" }

  open(10, iostat=u,form="formatted")
  open(10, iostat=u,form="unformatted")
  open(10, iostat=u,form="default") ! { dg-warning "FORM specifier in OPEN statement" }

  open(10, iostat=u,pad="yes")
  open(10, iostat=u,pad="no")
  open(10, iostat=u,pad=foo) ! { dg-warning "PAD specifier in OPEN statement" }

  open(10, iostat=u,position="asis")
  open(10, iostat=u,position="rewind")
  open(10, iostat=u,position="append")
  open(10, iostat=u,position=foo) ! { dg-warning "POSITION specifier in OPEN statement" }

  open(10, iostat=u,recl="ee") ! { dg-error "must be of type INTEGER" }
  open(10, iostat=u,recl=0.4) ! { dg-error "must be of type INTEGER" }
  open(10, iostat=u,recl=zero) ! { dg-warning "must be positive" }
  open(10, iostat=u,recl=mone) ! { dg-warning "must be positive" }

  open(10, iostat=u,status="unknown")
  open(10, iostat=u,status="old")
  open(10, iostat=u,status=foo) ! { dg-warning "STATUS specifier in OPEN statement" }
  
  open(10, iostat=u,status="new") ! { dg-warning "no FILE specifier is present" }
  open(10, iostat=u,status="replace   ") ! { dg-warning "no FILE specifier is present" }
  open(10, iostat=u,status="scratch",file=str) ! { dg-warning "cannot have the value SCRATCH if a FILE specifier is present" }

  open(10, iostat=u,form="unformatted",delim="none") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }
  open(10, iostat=u,form="unformatted",pad="yes") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }
  open(10, iostat=u,form="unformatted",blank="null") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }

  open(10, iostat=u,access="direct",position="append") ! { dg-warning "only allowed for stream or sequential ACCESS" }

  close(10, iostat=u,status="keep")
  close(10, iostat=u,status="delete")
  close(10, iostat=u,status=foo) ! { dg-warning "STATUS specifier in CLOSE statement" }
  close(iostat=u) ! { dg-error "requires a UNIT number" }



! Test for warnings, when an ERR label is specified

  open(10, err=99,access="sequential   ")
  open(10, err=99,access="sequential   u") ! { dg-warning "ACCESS specifier in OPEN statement" }
  open(10, err=99,access=foo) ! { dg-warning "ACCESS specifier in OPEN statement" }
  open(10, err=99,access="direct")
  open(10, err=99,access="stream")
  open(10, err=99,access="append") ! { dg-warning "Extension: ACCESS specifier in OPEN statement" }

  open(10, err=99,action="read")
  open(10, err=99,action="write")
  open(10, err=99,action="readwrite")
  open(10, err=99,action=foo) ! { dg-warning "ACTION specifier in OPEN statement" }

  open(10, err=99,blank="ZERO")
  open(10, err=99,blank="nUlL")
  open(10, err=99,blank="NULLL") ! { dg-warning "BLANK specifier in OPEN statement" }

  open(10, err=99,delim="apostrophe")
  open(10, err=99,delim="quote")
  open(10, err=99,delim="none")
  open(10, err=99,delim="") ! { dg-warning "DELIM specifier in OPEN statement" }

  open(10, err=99,form="formatted")
  open(10, err=99,form="unformatted")
  open(10, err=99,form="default") ! { dg-warning "FORM specifier in OPEN statement" }

  open(10, err=99,pad="yes")
  open(10, err=99,pad="no")
  open(10, err=99,pad=foo) ! { dg-warning "PAD specifier in OPEN statement" }

  open(10, err=99,position="asis")
  open(10, err=99,position="rewind")
  open(10, err=99,position="append")
  open(10, err=99,position=foo) ! { dg-warning "POSITION specifier in OPEN statement" }

  open(10, err=99,recl="ee") ! { dg-error "must be of type INTEGER" }
  open(10, err=99,recl=0.4) ! { dg-error "must be of type INTEGER" }
  open(10, err=99,recl=zero) ! { dg-warning "must be positive" }
  open(10, err=99,recl=mone) ! { dg-warning "must be positive" }

  open(10, err=99,status="unknown")
  open(10, err=99,status="old")
  open(10, err=99,status=foo) ! { dg-warning "STATUS specifier in OPEN statement" }
  
  open(10, err=99,status="new") ! { dg-warning "no FILE specifier is present" }
  open(10, err=99,status="replace   ") ! { dg-warning "no FILE specifier is present" }
  open(10, err=99,status="scratch",file=str) ! { dg-warning "cannot have the value SCRATCH if a FILE specifier is present" }

  open(10, err=99,form="unformatted",delim="none") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }
  open(10, err=99,form="unformatted",pad="yes") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }
  open(10, err=99,form="unformatted",blank="null") ! { dg-warning "not allowed in OPEN statement for unformatted I/O" }

  open(10, err=99,access="direct",position="append") ! { dg-warning "only allowed for stream or sequential ACCESS" }

  close(10, err=99,status="keep")
  close(10, err=99,status="delete")
  close(10, err=99,status=foo) ! { dg-warning "STATUS specifier in CLOSE statement" }

 99 continue

! Test for errors

  open(10,access="sequential   ")
  open(10,access="sequential   u") ! { dg-error "ACCESS specifier in OPEN statement" }
  open(10,access=foo) ! { dg-error "ACCESS specifier in OPEN statement" }
  open(10,access="direct")
  open(10,access="stream")
  open(10,access="append") ! { dg-warning "Extension: ACCESS specifier in OPEN statement" }

  open(10,action="read")
  open(10,action="write")
  open(10,action="readwrite")
  open(10,action=foo) ! { dg-error "ACTION specifier in OPEN statement" }

  open(10,blank="ZERO")
  open(10,blank="nUlL")
  open(10,blank="NULLL") ! { dg-error "BLANK specifier in OPEN statement" }

  open(10,delim="apostrophe")
  open(10,delim="quote")
  open(10,delim="none")
  open(10,delim="") ! { dg-error "DELIM specifier in OPEN statement" }

  open(10,form="formatted")
  open(10,form="unformatted")
  open(10,form="default") ! { dg-error "FORM specifier in OPEN statement" }

  open(10,pad="yes")
  open(10,pad="no")
  open(10,pad=foo) ! { dg-error "PAD specifier in OPEN statement" }

  open(10,position="asis")
  open(10,position="rewind")
  open(10,position="append")
  open(10,position=foo) ! { dg-error "POSITION specifier in OPEN statement" }

  open(10,recl="ee") ! { dg-error "must be of type INTEGER" }
  open(10,recl=0.4) ! { dg-error "must be of type INTEGER" }
  open(10,recl=zero) ! { dg-error "must be positive" }
  open(10,recl=mone) ! { dg-error "must be positive" }

  open(10,status="unknown")
  open(10,status="old")
  open(10,status=foo) ! { dg-error "STATUS specifier in OPEN statement" }
  
  open(10,status="new") ! { dg-error "no FILE specifier is present" }
  open(10,status="replace   ") ! { dg-error "no FILE specifier is present" }
  open(10,status="scratch",file=str) ! { dg-error "cannot have the value SCRATCH if a FILE specifier is present" }

  open(10,form="unformatted",delim="none") ! { dg-error "not allowed in OPEN statement for unformatted I/O" }
  open(10,form="unformatted",pad="yes") ! { dg-error "not allowed in OPEN statement for unformatted I/O" }
  open(10,form="unformatted",blank="null") ! { dg-error "not allowed in OPEN statement for unformatted I/O" }

  open(10,access="direct",position="append") ! { dg-error "only allowed for stream or sequential ACCESS" }

  close(10,status="keep")
  close(10,status="delete")
  close(10,status=foo) ! { dg-error "STATUS specifier in CLOSE statement" }
end
