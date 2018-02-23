! Program to test the INDEX intrinsic
program test
  character(len=10) a
  integer w
  if (index("FORTRAN", "R") .ne. 3) STOP 1
  if (index("FORTRAN", "R", .TRUE.) .ne. 5) STOP 2
  if (w ("FORTRAN") .ne. 3) STOP 3
end

function w(str)
  character(len=7) str
  integer w
  w = index(str, "R")
end

