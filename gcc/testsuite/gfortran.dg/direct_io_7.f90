! { dg-do run }
! PR 34405 - direct access prohibits ENDFILE, BACKSPACE and REWIND
program test
  implicit none
  integer :: ios
  character(len=80) :: msg
  open (95, access="direct", recl=4, status="scratch")
  write (95,rec=1) 'abcd'

  ios = 0
  msg = " "
  backspace (95,iostat=ios,iomsg=msg)
  if (ios == 0 .or. &
       msg /= "Cannot BACKSPACE a file opened for DIRECT access") STOP 1

  ios = 0
  msg = " "
  endfile (95,iostat=ios,iomsg=msg)
  if (ios == 0 .or. &
       msg /= "Cannot perform ENDFILE on a file opened for DIRECT access") &
       STOP 2

  ios = 0
  msg = " "
  rewind (95,iostat=ios,iomsg=msg)
  if (ios == 0 .or. &
       msg /= "Cannot REWIND a file opened for DIRECT access ") STOP 3

  close (95)
end program test

