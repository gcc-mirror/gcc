! { dg-do run }
!
! This tests that blank common works in modules. PR23270
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module global
  common a, b
  real    a, b
end module global
program blank_common
  use global
  common z
  complex z
  a = 999.0_4
  b = -999.0_4
  if (z.ne.cmplx (a,b)) STOP 1
end program blank_common
