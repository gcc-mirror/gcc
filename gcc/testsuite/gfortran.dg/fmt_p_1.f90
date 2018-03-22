! { dg-do run }
! PR32554 Bug in P formatting
! Test case from the bug reporter
program gfcbug66
  real(8) :: x = 1.0e-100_8
  character(50) :: outstr
  write (outstr,'(1X,2E12.3)')    x, 2 * x
  if (outstr.ne."    0.100E-99   0.200E-99") STOP 1
  ! Before patch 2 * x was put out wrong
  write (outstr,'(1X,1P,2E12.3)') x, 2 * x
  if (outstr.ne."    1.000-100   2.000-100") STOP 2
end program gfcbug66

