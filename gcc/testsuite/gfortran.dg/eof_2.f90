! { dg-do run }
! Check that end= and iostat= specifiers are honoured when both are used
program eof_2
  integer ierr, i

  open (11, status="SCRATCH")
  ierr = 0
  read (11, *, end=10, iostat=ierr) i
  call abort
10 continue
  if (ierr .ge. 0) call abort
end program

