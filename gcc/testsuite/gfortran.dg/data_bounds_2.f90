! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/35095 - Improve bounds checking for DATA with implied-do

program chkdata
  character(len=2), dimension(2,2) :: str
  data (str(i,1),i=1,3) / 'A','B','C' / ! { dg-error "above array upper bound" }
  data (str(j,2),j=0,2) / 'A','B','C' / ! { dg-error "below array lower bound" }
end program chkdata
