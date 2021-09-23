subroutine foo (x, msg1, msg2)
  integer x
  character(len=*) :: msg1, msg2
  if (x == 0) then
      !$omp error at(execution)
  else if (x == 1) then
      !$omp error severity (warning), at (execution)
  else if (x == 2) then
      !$omp error at ( execution ) severity (fatal) message ("baz")
  else if (x == 3) then
      !$omp error severity(warning) message (msg1) at(execution)
  else
      !$omp error message (msg2), at(execution), severity(fatal)
  end if
end
