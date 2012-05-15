! { dg-do run }
! { dg-options "-O2 -ffast-math -mfpmath=387" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
! { dg-options "-O2 -ffast-math" }

module scc_m
  implicit none
  integer, parameter :: dp = selected_real_kind(15,90)
contains
  subroutine self_ind_cir_coil (r, l, turns, mu, self_l)
  implicit none
  real (kind = dp), intent(in) :: r, l, turns, mu
  real (kind = dp), intent(out) :: self_l
  real (kind = dp) :: alpha, modulus, pk, ak, bk, ae, be, elliptice, elliptick
  real (kind = dp) :: expected
  alpha = atan(2.0_dp*r/l)
  modulus = sin(alpha)
  pk = 1.0_dp - modulus**2
  ak = (((0.01451196212_dp*pk+0.03742563713_dp)*pk+ &
         0.03590092383_dp)*pk+0.09666344259_dp)*pk+1.38629436112_dp
  bk = (((0.00441787012_dp*pk+0.03328355346_dp)*pk+ &
         0.06880248576_dp)*pk+0.12498593597_dp)*pk+0.5_dp
  elliptick = ak - bk * log(pk)
  ae = (((0.01736506451_dp*pk+0.04757383546_dp)*pk+ &
         0.0626060122_dp)*pk+0.44325141463_dp)*pk+1.0_dp
  be = (((0.00526449639_dp*pk+0.04069697526_dp)*pk+ &
         0.09200180037_dp)*pk+0.2499836831_dp)*pk
  elliptice = ae - be * log(pk)
  self_l = (mu * turns**2 * l**2 * 2.0_dp * r)/3.0_dp * &
           (((tan(alpha)**2-1.0_dp)*elliptice+elliptick)/sin(alpha) - &
            tan(alpha)**2)
  expected = 3.66008420600434162E-002_dp
  if (abs(self_l - expected) / expected > 1e-3) &
        call abort
  end subroutine self_ind_cir_coil
end module scc_m

program test
  use scc_m
  implicit none

  real (kind = dp) :: mu, turns, r, l, self_l
  mu = 1.25663706143591729E-006_dp
  turns = 166666.66666666666_dp
  l = 3.00000000000000006E-003_dp
  r = 2.99999999999999989E-002_dp

  call self_ind_cir_coil (r, l, turns, mu, self_l)
end program test
