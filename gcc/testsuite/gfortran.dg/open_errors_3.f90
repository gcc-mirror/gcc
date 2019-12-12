! { dg-do  run }
! PR 90461 Open file on multiple units as of F2018
program openmult
  implicit none
  character(len=*), parameter :: fname="pr90461.dat"
  open(10, file=fname, form="unformatted")
  open(11, file=fname, form="unformatted")
  close(11)
  close(10, status="delete")
end program openmult
! { dg-final { remote_file build delete "pr90461.dat" } }
