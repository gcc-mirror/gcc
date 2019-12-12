! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
!
! PR fortran/34318
!
! Infinity and NaN were not properly written to the .mod file.
!
module nonordinal
  implicit none
  real, parameter :: inf = 1./0., nan = 0./0., minf = -1./0.0
end module nonordinal

program a
  use nonordinal
  implicit none
  character(len=20) :: str
  if (log(abs(inf))  < huge(inf)) STOP 1
  if (log(abs(minf)) < huge(inf)) STOP 2
  if (.not. isnan(nan)) STOP 3
  write(str,"(sp,f10.2)") inf
  if (adjustl(str) /= "+Infinity") STOP 4
  write(str,*) minf
  if (adjustl(str) /= "-Infinity") STOP 5
  write(str,*) nan
  if (adjustl(str) /= "NaN") STOP 6
end program a
