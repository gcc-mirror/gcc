! { dg-do run }
! PR65234  Output descriptor (*(1E15.7)) not accepted
program IOtest
  character(40) :: str
  double precision :: d = 5.0
  write (str, '(*(2(E15.7)))') d, d
  if (str /= "  0.5000000E+01  0.5000000E+01") STOP 1
  write (str, '(*(2E15.7))') d, d
  if (str /= "  0.5000000E+01  0.5000000E+01") STOP 2
end program
