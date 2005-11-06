! { dg-do run }
! Test implementation of the iomsg tag.
program iomsg_test
  character(len=70) ch

  ! Test that iomsg is left unchanged with no error
  ch = 'asdf'
  open(10, status='scratch', iomsg=ch, iostat=i) ! { dg-warning "Fortran 2003: IOMSG tag" }
  if (ch .ne. 'asdf') call abort

  ! Test iomsg with data transfer statement
  read(10,'(I2)', iomsg=ch, end=100) k ! { dg-warning "Fortran 2003: IOMSG tag" }
  call abort
100 continue
  if (ch .ne. 'End of file') call abort

  ! Test iomsg with open
  open (-3, err=200, iomsg=ch) ! { dg-warning "Fortran 2003: IOMSG tag" }

  call abort
200 continue
  if (ch .ne. 'Bad unit number in OPEN statement') call abort

  ! Test iomsg with close
  close(23,status="no_idea", err=500, iomsg=ch) ! { dg-warning "Fortran 2003: IOMSG tag" }
500 continue
  if (ch .ne. "Bad STATUS parameter in CLOSE statement") call abort
end program iomsg_test
