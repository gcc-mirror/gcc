! { dg-do run { target fd_truncate } }
!
! PR fortran/34530
!
! Skipping over comment line was not working
!
! Test case contributed by Harald Anlauf.
!
program gfcbug77
  implicit none

  character(len=128) :: file = ""
  logical            :: default
  namelist /BLACKLIST/ file, default
  integer, parameter :: nnml = 10
  default = .true.

  open (nnml, file='gfcbug77.nml')
  write(nnml,*) "&blacklist "           ! The trailing space breaks gfortran
  write(nnml,*) "  ! This is a comment within the namelist"
  write(nnml,*) "  file    = 'myfile'"
  write(nnml,*) "  default = F"
  write(nnml,*) "/"
  rewind(nnml)
  read (nnml, nml=BLACKLIST)
  close(nnml,status="delete")
  if(file /= "myfile" .or. default) call abort()
!  write (*,nml=BLACKLIST)
end program gfcbug77
