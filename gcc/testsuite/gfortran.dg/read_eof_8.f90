! { dg-do run }
! PR43265: See comment #26 in the PR.  Before patch,
! the test case would fail to read the last line of the file.
! Thanks to Jean-Baptiste Faure for providing the initial test case.
program test
  character (len=6) :: line
  integer :: n, k=0
  open(unit=25,file="test.dat",status="replace", &
       & form="unformatted", access="stream")
  write(25) "Line 1" // char(10)
  write(25) "Line 2" // char(10)
  write(25) "Line 3" // char(10)
  write(25) "Line 4" // char(10)
  write(25) "Line 5" ! No EOR marker on the last line.
  close(25, status="keep")
  open(25, file="test.dat", status="old")
  do n=1,10
   read(25,'(a)',end=100,err=101) line
   k = k+1
  enddo
  call abort
100 if (k /= 5) call abort
  stop
101 call abort
end program test

