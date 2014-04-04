! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-require-effective-target fortran_real_16 }
! { dg-require-effective-target fortran_integer_16 }
! { dg-skip-if "" { "powerpc*le-*-*" } { "*" } { "" } }
! PR47293 NAN not correctly read
character(len=200) :: str
real(16) :: r
integer(16) :: k2
integer(16), parameter :: quietnan = 170099645085600953110659059745250344960
r = 1.0
str = 'NAN' ; read(str,*) r
k2 = transfer(r,k2)
k2 = iand(k2, z'fff80000000000000000000000000000')
if (k2.ne.quietnan) call abort
end
