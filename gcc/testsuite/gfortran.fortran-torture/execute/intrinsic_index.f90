! Program to test the INDEX intrinsic
program test
  character(len=10) a
  integer w
  if (index("FORTRAN", "R") .ne. 3) call abort
  if (index("FORTRAN", "R", .TRUE.) .ne. 5) call abort
  if (w ("FORTRAN") .ne. 3) call abort
end

function w(str)
  character(len=8) str
  integer w
  w = index(str, "R")
end

