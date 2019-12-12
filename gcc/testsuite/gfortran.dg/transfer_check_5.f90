! { dg-do compile }
! { dg-options "-Wsurprising" }
!
! PR fortran/89516 - ICE in gfc_calculate_transfer_sizes at gcc/fortran/check.c:5506
! Found by Martin Liška

program test
  character(*), parameter :: n = ''
  character(*), parameter :: o = transfer ([''], n)
  print *, transfer(1,'',size=0) ! No warning
  print *, transfer(1,'',size=1) ! No warning
  print *, transfer('',1,size=0) ! No warning
  print *, transfer('',1,size=1) ! { dg-warning "has partly undefined result" }
end program test
