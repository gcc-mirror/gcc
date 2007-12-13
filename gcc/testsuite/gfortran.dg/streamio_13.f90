! { dg-do run }
! PR 34405 - BACKSPACE for unformatted stream files is prohibited.
program main
  implicit none
  integer :: ios
  character(len=80) :: msg
  open(2003,form="unformatted",access="stream",status="scratch")
  write (2003) 1
  write (2003) 2
  ios = 0
  msg = ' '
  backspace (2003,iostat=ios,iomsg=msg)
  if (ios == 0 .or. msg /="Cannot BACKSPACE an unformatted stream file") &
       call abort
end program main
