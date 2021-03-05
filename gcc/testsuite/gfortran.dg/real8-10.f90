! { dg-do run { target i?86-*-* x86_64-*-* } }
! { dg-additional-options "-w -freal-8-real-10" }
!
! PR fortran/99355
! PR fortran/99355 comment 10 to 13 + PR fortran/57871
!

program test
  real :: r1
  real*4:: r2
  real(4) :: r3
  real(selected_real_kind(p=6)) :: r4
  integer, parameter :: k4 = 4, k8 = 8

  double precision :: d1
  real*8 :: d2
  real(8) :: d3
  real(kind(1.d0)) :: d4
  real(selected_real_kind(p=15)) :: d5

  !print '(tr3,a10,10(tr1,i2))', 'single', kind(r1), kind(r2), kind(r3), kind(r4)
  !print '(tr3,a10,10(tr1,i2))', 'double', kind(d1), kind(d2), kind(d3), kind(d4), kind(d5)
  if (any ([kind(1.0), kind(1.0_4), kind(1.0_k4), kind(r1), kind(r2), kind(r3), kind(r4)] /= 4)) stop 1
  if (any ([kind(1.d0), kind(1.0_8), kind(1.0_k8), kind(d1), kind(d2), kind(d3), kind(d4), kind(d5)] /= 10)) stop 2
end program test
