! { dg-do run }
! PR 18982:  verifies that opening an existing file with
!            status="new" is an error
program main
  nout = 10
  open(nout, file="foo_open_new.dat", status="replace")     ! make sure foo_open_new.dat exists
  close(nout)
  open(nout, file="foo_open_new.dat", status="new",err=100)
  STOP 1! This should never happen
100 call unlink ("foo_open_new.dat")
end program main
