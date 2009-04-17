! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-options "-fno-range-check -mieee" { target alpha*-*-* sh*-*-* } }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
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
  if (log(abs(inf))  < huge(inf)) call abort()
  if (log(abs(minf)) < huge(inf)) call abort()
  if (.not. isnan(nan)) call abort()
  write(str,*) inf
  if (adjustl(str) /= "+Infinity") call abort()
  write(str,*) minf
  if (adjustl(str) /= "-Infinity") call abort()
  write(str,*) nan
  if (adjustl(str) /= "NaN") call abort()
end program a

! { dg-final { cleanup-modules "nonordinal" } }
