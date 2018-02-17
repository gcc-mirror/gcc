! { dg-do run }
! Check for correct ordering of character variables with CONVERT

program main
  implicit none
  integer, parameter :: two_swap = 2**25
  integer(kind=4) i,j
  character(len=2) :: c,d
  open(20,file="convert.dat",form="unformatted",convert="swap") ! { dg-warning "CONVERT" }
  write (20) "ab"
  close (20)
  open(20,file="convert.dat",form="unformatted",access="stream")
  read(20) i,c,j
  if (i .ne. two_swap .or. j .ne. two_swap .or. c .ne. "ab") STOP 1
  close (20)
  open(20,file="convert.dat",form="unformatted",convert="swap") ! { dg-warning "CONVERT" }
  read (20) d
  close (20,status="delete")
  if (d .ne. "ab") STOP 2
end program main
