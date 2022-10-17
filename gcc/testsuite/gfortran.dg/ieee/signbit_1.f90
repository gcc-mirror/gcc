! Test IEEE_SIGNBIT
! { dg-do run }

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_exceptions
  use, intrinsic :: ieee_arithmetic
  implicit none

  real :: sx1
  double precision :: dx1

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k1) :: xk1
  real(kind=k2) :: xk2

  ! Float

  sx1 = 1.3
  if (ieee_signbit(sx1)) stop 1
  sx1 = huge(sx1)
  if (ieee_signbit(sx1)) stop 2
  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_signbit(sx1)) stop 3
  sx1 = tiny(sx1)
  if (ieee_signbit(sx1)) stop 4
  sx1 = tiny(sx1)
  sx1 = sx1 / 101
  if (ieee_signbit(sx1)) stop 5
  sx1 = 0
  if (ieee_signbit(sx1)) stop 6
  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_signbit(sx1)) stop 7

  sx1 = -1.3
  if (.not. ieee_signbit(sx1)) stop 8
  sx1 = -huge(sx1)
  if (.not. ieee_signbit(sx1)) stop 9
  sx1 = -ieee_value(sx1, ieee_positive_inf)
  if (.not. ieee_signbit(sx1)) stop 10
  sx1 = -tiny(sx1)
  if (.not. ieee_signbit(sx1)) stop 11
  sx1 = -tiny(sx1)
  sx1 = sx1 / 101
  if (.not. ieee_signbit(sx1)) stop 12
  sx1 = 0
  sx1 = -sx1
  if (.not. ieee_signbit(sx1)) stop 13
  sx1 = ieee_value(sx1, ieee_quiet_nan)
  sx1 = -sx1
  if (.not. ieee_signbit(sx1)) stop 14

  ! Double

  dx1 = 1.3
  if (ieee_signbit(dx1)) stop 1
  dx1 = huge(dx1)
  if (ieee_signbit(dx1)) stop 2
  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_signbit(dx1)) stop 3
  dx1 = tiny(dx1)
  if (ieee_signbit(dx1)) stop 4
  dx1 = tiny(dx1)
  dx1 = dx1 / 101
  if (ieee_signbit(dx1)) stop 5
  dx1 = 0
  if (ieee_signbit(dx1)) stop 6
  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_signbit(dx1)) stop 7

  dx1 = -1.3
  if (.not. ieee_signbit(dx1)) stop 8
  dx1 = -huge(dx1)
  if (.not. ieee_signbit(dx1)) stop 9
  dx1 = -ieee_value(dx1, ieee_positive_inf)
  if (.not. ieee_signbit(dx1)) stop 10
  dx1 = -tiny(dx1)
  if (.not. ieee_signbit(dx1)) stop 11
  dx1 = -tiny(dx1)
  dx1 = dx1 / 101
  if (.not. ieee_signbit(dx1)) stop 12
  dx1 = 0
  dx1 = -dx1
  if (.not. ieee_signbit(dx1)) stop 13
  dx1 = ieee_value(dx1, ieee_quiet_nan)
  dx1 = -dx1
  if (.not. ieee_signbit(dx1)) stop 14

  ! Large kind 1

  xk1 = 1.3
  if (ieee_signbit(xk1)) stop 1
  xk1 = huge(xk1)
  if (ieee_signbit(xk1)) stop 2
  xk1 = ieee_value(xk1, ieee_positive_inf)
  if (ieee_signbit(xk1)) stop 3
  xk1 = tiny(xk1)
  if (ieee_signbit(xk1)) stop 4
  xk1 = tiny(xk1)
  xk1 = xk1 / 101
  if (ieee_signbit(xk1)) stop 5
  xk1 = 0
  if (ieee_signbit(xk1)) stop 6
  xk1 = ieee_value(xk1, ieee_quiet_nan)
  if (ieee_signbit(xk1)) stop 7

  xk1 = -1.3
  if (.not. ieee_signbit(xk1)) stop 8
  xk1 = -huge(xk1)
  if (.not. ieee_signbit(xk1)) stop 9
  xk1 = -ieee_value(xk1, ieee_positive_inf)
  if (.not. ieee_signbit(xk1)) stop 10
  xk1 = -tiny(xk1)
  if (.not. ieee_signbit(xk1)) stop 11
  xk1 = -tiny(xk1)
  xk1 = xk1 / 101
  if (.not. ieee_signbit(xk1)) stop 12
  xk1 = 0
  xk1 = -xk1
  if (.not. ieee_signbit(xk1)) stop 13
  xk1 = ieee_value(xk1, ieee_quiet_nan)
  xk1 = -xk1
  if (.not. ieee_signbit(xk1)) stop 14

  ! Large kind 2

  xk2 = 1.3
  if (ieee_signbit(xk2)) stop 1
  xk2 = huge(xk2)
  if (ieee_signbit(xk2)) stop 2
  xk2 = ieee_value(xk2, ieee_positive_inf)
  if (ieee_signbit(xk2)) stop 3
  xk2 = tiny(xk2)
  if (ieee_signbit(xk2)) stop 4
  xk2 = tiny(xk2)
  xk2 = xk2 / 101
  if (ieee_signbit(xk2)) stop 5
  xk2 = 0
  if (ieee_signbit(xk2)) stop 6
  xk2 = ieee_value(xk2, ieee_quiet_nan)
  if (ieee_signbit(xk2)) stop 7

  xk2 = -1.3
  if (.not. ieee_signbit(xk2)) stop 8
  xk2 = -huge(xk2)
  if (.not. ieee_signbit(xk2)) stop 9
  xk2 = -ieee_value(xk2, ieee_positive_inf)
  if (.not. ieee_signbit(xk2)) stop 10
  xk2 = -tiny(xk2)
  if (.not. ieee_signbit(xk2)) stop 11
  xk2 = -tiny(xk2)
  xk2 = xk2 / 101
  if (.not. ieee_signbit(xk2)) stop 12
  xk2 = 0
  xk2 = -xk2
  if (.not. ieee_signbit(xk2)) stop 13
  xk2 = ieee_value(xk2, ieee_quiet_nan)
  xk2 = -xk2
  if (.not. ieee_signbit(xk2)) stop 14

end
