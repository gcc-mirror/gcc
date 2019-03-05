! { dg-do  run }
! PR 88658 - make sure the types for min1, max1, amax0 and amin0 are
! correct when simplified

program main
  real :: RVCOMP
  character (len=12) :: line
  integer :: n

  RVCOMP = MAX1(2.3, 3.1, 4.4) / 5 
  if (rvcomp /= 0.) stop 1
  rvcomp = min1(2.3, 3.1, 5.1) / 5
  if (rvcomp /= 0.) stop 2
  write (unit=line, fmt='(F12.5)') amax0(42, 21, 7)
  if (line /= '    42.00000') stop 3
  write (unit=line, fmt='(F12.5)') amin0(42,21,7)
  if (line /= '     7.00000') stop 4
end program main
