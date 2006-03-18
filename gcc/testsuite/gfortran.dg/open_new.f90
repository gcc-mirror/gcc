! { dg-do run }
! PR 18982:  verifies that opening an existing file with
!            status="new" is an error
program main
  nout = 10
  open(nout, file="foo.dat", status="replace")     ! make sure foo.dat exists
  close(nout)
  open(nout, file="foo.dat", status="new",err=100)
  call abort                 ! This should never happen
100 call unlink ("foo.dat")
end program main
