! { dg-do run { target fd_truncate } }
! PR34411 hang-up during read of non-expected input
! Test case derived from that given in PR
! Prior to patch, the do loop was infinite, limits set in this one
program pr34411
  real :: x,y
  ii = 0
  iostat = 0
  x = 0.0; y= 0.0
  open (10, status="scratch")
  write (10, '(a)')" 289  329.142  214.107   12.313   12.050   11.913   11.868"
  write (10, '(a)')"  2038.497 99.99  0.00   0.019    0.021    0.025    0.034"
  write (10, '(a)')""
  write (10, '(a)')" 413  360.334  245.261   12.375   11.910   11.469   11.086"
  write (10, '(a)')"  2596.395 99.99  0.00   0.019    0.017    0.016    0.015" 
  write (10, '(a)')""
  write (10, '(a)')" 655  332.704  317.964   12.523   12.212   11.998   11.892"
  write (10, '(a)')"  1627.586 99.99  0.00   0.005    0.005    0.006    0.007"
  write (10, '(a)')""
  write (10, '(a)')" 360  379.769  231.226   12.709   12.422   12.195   11.941"
  write (10, '(a)')"  2561.539 99.99  0.00   0.042    0.043    0.050    0.055"
  rewind 10
  do i = 1,100
     read(10,'(T7,2F9.3)', iostat=ii, end=666) x,y
  end do
666 continue
  if (i /= 12) call abort
  if (x /= 379.76901 .and. y /= 231.22600) call abort
  close(10)
end program pr34411
