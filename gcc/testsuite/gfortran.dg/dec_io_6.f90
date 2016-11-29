! { dg-do run }
! { dg-options "-fdec" }
!
! Test that we get a run-time warning for close-on-delete with READONLY,
! and that the file is protected from deletion.
!

implicit none

integer :: fd = 8
character(*), parameter :: f = "dec_io_6.txt"
logical :: exists

open(unit=fd,file=f,action='write')
close(unit=fd)

open(unit=fd,file=f,action='read',readonly)
close(unit=fd,status='delete') ! { dg-output "file protected by READONLY" }

inquire(file=f, EXIST=exists)
if (.not. exists) then
  print *, 'file was not protected by READONLY!'
  call abort()
endif

open(unit=fd,file=f,action='write')
close(unit=fd,status='delete') ! cleanup

end
