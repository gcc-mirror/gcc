! { dg-do compile }
! { dg-options "-std=f95" }
! Testcase for PR libfortran/25068
  real :: u
  integer(kind=8) :: i
  open (10,status="scratch")
  read (10,*,iostat=i) u ! { dg-error "Fortran 95 requires default INTEGER in IOSTAT tag" }
  close (10,iostat=i) ! { dg-error "Fortran 95 requires default INTEGER in IOSTAT tag" }
  end
