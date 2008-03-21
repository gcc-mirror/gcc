! { dg-do run }
! PR35617 read namelist error with '!'
program test
  character(len=128) :: mhdpath
  namelist /nbdrive_naml/ mhdpath
  open(10, file='test.nml')
  
  write(10,'(a)') "&nbdrive_naml"
  write(10,'(a)') 
  write(10,'(a)') "!nstep_stop = 2  ! uncomment to bar"
  write(10,'(a)') "!nstep_start = 2 ! uncomment to foo"
  write(10,'(a)') " mhdpath = 'mypath.dat'"
  write(10,'(a)') "/"

  rewind(10)
  read(10, nbdrive_naml)
  close(10,status="delete")
end program test
