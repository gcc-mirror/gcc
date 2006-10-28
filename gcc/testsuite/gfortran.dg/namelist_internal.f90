! { dg-do run }
! { dg-options "-fall-intrinsics -std=f2003" }
! Checks internal file read/write of namelists
! (Fortran 2003 feature)
! PR fortran/28224
program nml_internal
  integer   :: i, j
  real      :: r
  namelist /nam/ i, j, r
  character(len=250) :: str

  i = 42
  j = -718
  r = exp(1.0)
  write(str,nml=nam)
  i = -33
  j = 10
  r = sin(1.0)
  read(str,nml=nam)
  if(i /= 42 .or. j /= -718 .or. abs(r-exp(1.0)) > 1e-5) call abort()
end program nml_internal
