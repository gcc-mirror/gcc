! Test IEEE_FMA
! { dg-do run }

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_exceptions
  use, intrinsic :: ieee_arithmetic
  implicit none

  integer :: ex

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k1) :: lx1, lx2, lx3
  real(kind=k2) :: wx1, wx2, wx3

  ! Float

  sx1 = 3 ; sx2 = 2 ; sx3 = 1
  if (ieee_fma(sx1, sx2, sx3) /= 7) stop 1
  sx1 = 0 ; sx2 = 2 ; sx3 = 1
  if (ieee_fma(sx1, sx2, sx3) /= 1) stop 2
  sx1 = 3 ; sx2 = 2 ; sx3 = 0
  if (ieee_fma(sx1, sx2, sx3) /= 6) stop 3

  ex = int(log(rrspacing(real(1, kind(sx1)))) / log(real(2, kind(sx1)))) - 1
  sx1 = 1 + spacing(real(1, kind(sx1)))
  sx2 = 2 ; sx2 = sx2 ** ex ; sx2 = sx2 * 3
  sx3 = -sx2

  print *, sx1 * sx2 + sx3
  print *, ieee_fma(sx1, sx2, sx3)
  if (ieee_fma(sx1, sx2, sx3) /= real(3, kind(sx1)) / 2) stop 4

  ! Double

  dx1 = 3 ; dx2 = 2 ; dx3 = 1
  if (ieee_fma(dx1, dx2, dx3) /= 7) stop 1
  dx1 = 0 ; dx2 = 2 ; dx3 = 1
  if (ieee_fma(dx1, dx2, dx3) /= 1) stop 2
  dx1 = 3 ; dx2 = 2 ; dx3 = 0
  if (ieee_fma(dx1, dx2, dx3) /= 6) stop 3

  ex = int(log(rrspacing(real(1, kind(dx1)))) / log(real(2, kind(dx1)))) - 1
  dx1 = 1 + spacing(real(1, kind(dx1)))
  dx2 = 2 ; dx2 = dx2 ** ex ; dx2 = dx2 * 3
  dx3 = -dx2

  print *, dx1 * dx2 + dx3
  print *, ieee_fma(dx1, dx2, dx3)
  if (ieee_fma(dx1, dx2, dx3) /= real(3, kind(dx1)) / 2) stop 4

  ! Large kind 1

  lx1 = 3 ; lx2 = 2 ; lx3 = 1
  if (ieee_fma(lx1, lx2, lx3) /= 7) stop 1
  lx1 = 0 ; lx2 = 2 ; lx3 = 1
  if (ieee_fma(lx1, lx2, lx3) /= 1) stop 2
  lx1 = 3 ; lx2 = 2 ; lx3 = 0
  if (ieee_fma(lx1, lx2, lx3) /= 6) stop 3

  ex = int(log(rrspacing(real(1, kind(lx1)))) / log(real(2, kind(lx1)))) - 1
  lx1 = 1 + spacing(real(1, kind(lx1)))
  lx2 = 2 ; lx2 = lx2 ** ex ; lx2 = lx2 * 3
  lx3 = -lx2

  print *, lx1 * lx2 + lx3
  print *, ieee_fma(lx1, lx2, lx3)
  if (ieee_fma(lx1, lx2, lx3) /= real(3, kind(lx1)) / 2) stop 4

  ! Large kind 2

  wx1 = 3 ; wx2 = 2 ; wx3 = 1
  if (ieee_fma(wx1, wx2, wx3) /= 7) stop 1
  wx1 = 0 ; wx2 = 2 ; wx3 = 1
  if (ieee_fma(wx1, wx2, wx3) /= 1) stop 2
  wx1 = 3 ; wx2 = 2 ; wx3 = 0
  if (ieee_fma(wx1, wx2, wx3) /= 6) stop 3

  ex = int(log(rrspacing(real(1, kind(wx1)))) / log(real(2, kind(wx1)))) - 1
  wx1 = 1 + spacing(real(1, kind(wx1)))
  wx2 = 2 ; wx2 = wx2 ** ex ; wx2 = wx2 * 3
  wx3 = -wx2

  print *, wx1 * wx2 + wx3
  print *, ieee_fma(wx1, wx2, wx3)
  if (ieee_fma(wx1, wx2, wx3) /= real(3, kind(wx1)) / 2) stop 4

end
