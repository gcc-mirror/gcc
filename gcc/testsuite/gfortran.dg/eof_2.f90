! { dg-do run }
! Check that end= and iostat= specifiers are honoured when both are used
program eof_2
  integer ierr, i

  open (11, status="SCRATCH")
  ierr = 0
  read (11, *, end=10, iostat=ierr) i
  STOP 1
10 continue
  if (ierr .ge. 0) STOP 2
end program

