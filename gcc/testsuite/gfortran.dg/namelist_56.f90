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
  write(99,nml)
  rewind(99)
  j = -54
  str = 'XXXX'
  read(99,nml)
  if (j.ne.5) call abort
  if (any(str.ne.["a    ","b    ","cde  ","     "])) call abort
  close(99,status="delete")
end
