! { dg-do run }
! PR37707 Namelist read of array of derived type incorrect
! Test case from Tobias Burnus
  IMPLICIT NONE
  integer :: j
  character(len=5) :: str(4)
  character(len=900) :: nlstr
  namelist /nml/ str, j
  str = ''
  j = -42
  nlstr = '&nml str = "a", "b", "cde", j = 5 /'
  read(nlstr,nml)
  open(99, status="scratch")
  write(99,nml)
  rewind(99)
  j = -54
  str = 'XXXX'
  read(99,nml)
  if (j.ne.5) STOP 1
  if (any(str.ne.["a    ","b    ","cde  ","     "])) STOP 2
  close(99)
end
