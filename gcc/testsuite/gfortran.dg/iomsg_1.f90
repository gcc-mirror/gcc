! { dg-do run }
! Test implementation of the iomsg tag.
program iomsg_test
  character(len=70) ch

  ! Test that iomsg is left unchanged with no error
  ch = 'asdf'
  open(10, status='scratch', iomsg=ch, iostat=i)
  if (ch .ne. 'asdf') STOP 1

  ! Test iomsg with data transfer statement
  read(10,'(I2)', iomsg=ch, end=100) k
  STOP 2
100 continue
  if (ch .ne. 'End of file') STOP 3

  ! Test iomsg with open
  open (-3, err=200, iomsg=ch)

  STOP 4
200 continue
  if (ch .ne. 'Bad unit number in OPEN statement') STOP 5

  ! Test iomsg with close
  close(23,status="no_idea", err=500, iomsg=ch) ! { dg-warning "STATUS specifier in CLOSE statement.*has invalid value" }
500 continue
  if (ch .ne. "Bad STATUS parameter in CLOSE statement") STOP 6
end program iomsg_test
