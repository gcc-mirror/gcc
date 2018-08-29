! { dg-do run }
! PR84412 Wrong "Inquire statement identifies an internal file" error 
program bug
  implicit none
  integer          :: i
  character(len=1) :: s
  write (s,'(i1)') 0
  open(newUnit=i,file='inquire_18.txt',status='unknown')
  inquire(unit=i)
  close(i, status="delete")
end program bug
