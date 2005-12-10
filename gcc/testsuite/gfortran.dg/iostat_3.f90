! { dg-do compile }
! Testcase for PR libfortran/25068
  real :: u
  integer(kind=8) :: i
  open (10,status="scratch")
  read (10,*,iostat=i) u ! { dg-warning "Fortran 2003: Non-default integer kind in IOSTAT tag" }
  close (10,iostat=i) ! { dg-warning "Fortran 2003: Non-default integer kind in IOSTAT tag" }
  end
