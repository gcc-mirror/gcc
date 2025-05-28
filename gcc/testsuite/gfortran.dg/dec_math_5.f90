! { dg-do run }
! { dg-additional-options "-std=gnu" }
! { dg-require-effective-target fortran_real_10 }
! { dg-require-effective-target fortran_real_16 }

program p
  implicit none
  integer, parameter :: ep = selected_real_kind (17) ! real(10)
  real(4)  :: a1, e1 = 1.e-5
  real(8)  :: b1, e2 = 1.e-14
  real(ep) :: c1, e3 = 1.e-17
  real(16) :: d1, e4 = 1.e-30

  a1 = 1; a1 = atand(a1)
  b1 = 1; b1 = atand(b1)
  c1 = 1; c1 = atand(c1)
  d1 = 1; d1 = atand(d1)
! print '(4(F15.11))', a1, b1, c1, d1
  if (abs(a1 - 45) > e1) stop 1
  if (abs(b1 - 45) > e2) stop 2
  if (abs(c1 - 45) > e3) stop 3
  if (abs(d1 - 45) > e4) stop 4

  a1 = 0.5; a1 = asind(a1)
  b1 = 0.5; b1 = asind(b1)
  c1 = 0.5; c1 = asind(c1)
  d1 = 0.5; d1 = asind(d1)
  if (abs(a1 - 30) > e1) stop 5
  if (abs(b1 - 30) > e2) stop 6
  if (abs(c1 - 30) > e3) stop 7
  if (abs(d1 - 30) > e4) stop 8

  a1 = 0.5; a1 = acosd(a1)
  b1 = 0.5; b1 = acosd(b1)
  c1 = 0.5; c1 = acosd(c1)
  d1 = 0.5; d1 = acosd(d1)
  if (abs(a1 - 60) > e1) stop 9
  if (abs(b1 - 60) > e2) stop 10
  if (abs(c1 - 60) > e3) stop 11
  if (abs(d1 - 60) > e4) stop 12

  a1 = 45; a1 = tand(a1)
  b1 = 45; b1 = tand(b1)
  c1 = 45; c1 = tand(c1)
  d1 = 45; d1 = tand(d1)
  if (abs(a1 - 1) > e1) stop 13
  if (abs(b1 - 1) > e2) stop 14
  if (abs(c1 - 1) > e3) stop 15
  if (abs(d1 - 1) > e4) stop 16

  a1 = 60; a1 = tand(a1)
  b1 = 60; b1 = tand(b1)
  c1 = 60; c1 = tand(c1)
  d1 = 60; d1 = tand(d1)
  if (abs(a1 - sqrt (3._4) ) > e1) stop 17
  if (abs(b1 - sqrt (3._8) ) > e2) stop 18
  if (abs(c1 - sqrt (3._ep)) > e3) stop 19
  if (abs(d1 - sqrt (3._16)) > e4) stop 20

  a1 = 45; a1 = cotand(a1)
  b1 = 45; b1 = cotand(b1)
  c1 = 45; c1 = cotand(c1)
  d1 = 45; d1 = cotand(d1)
  if (abs(a1 - 1) > e1) stop 21
  if (abs(b1 - 1) > e2) stop 22
  if (abs(c1 - 1) > e3) stop 23
  if (abs(d1 - 1) > e4) stop 24

  a1 = 30; a1 = cotand(a1)
  b1 = 30; b1 = cotand(b1)
  c1 = 30; c1 = cotand(c1)
  d1 = 30; d1 = cotand(d1)
  if (abs(a1 - sqrt (3._4) ) > e1) stop 25
  if (abs(b1 - sqrt (3._8) ) > e2) stop 26
  if (abs(c1 - sqrt (3._ep)) > e3) stop 27
  if (abs(d1 - sqrt (3._16)) > e4) stop 28

  a1 = 1; a1 = atan2d(a1, a1)
  b1 = 1; b1 = atan2d(b1, b1)
  c1 = 1; c1 = atan2d(c1, c1)
  d1 = 1; d1 = atan2d(d1, d1)
  if (abs(a1 - 45) > e1) stop 29
  if (abs(b1 - 45) > e2) stop 30
  if (abs(c1 - 45) > e3) stop 31
  if (abs(d1 - 45) > e4) stop 32

  a1 = 30; a1 = sind(a1)
  b1 = 30; b1 = sind(b1)
  c1 = 30; c1 = sind(c1)
  d1 = 30; d1 = sind(d1)
  if (abs(a1 - 0.5) > e1) stop 33
  if (abs(b1 - 0.5) > e2) stop 34
  if (abs(c1 - 0.5) > e3) stop 35
  if (abs(d1 - 0.5) > e4) stop 36

  a1 = 60; a1 = cosd(a1)
  b1 = 60; b1 = cosd(b1)
  c1 = 60; c1 = cosd(c1)
  d1 = 60; d1 = cosd(d1)
  if (abs(a1 - 0.5) > e1) stop 37
  if (abs(b1 - 0.5) > e2) stop 38
  if (abs(c1 - 0.5) > e3) stop 39
  if (abs(d1 - 0.5) > e4) stop 40

  a1 = acospi(0.5)
  b1 = acospi(-0.5)
  c1 = acospi(0.5)
  d1 = acospi(-0.5)
  if (abs(a1 - 1.0 / 3) > e1) stop 41
  if (abs(b1 - 2.0 / 3) > e2) stop 42
  if (abs(c1 - 1.0 / 3) > e3) stop 43
  if (abs(d1 - 2.0 / 3) > e4) stop 44

  a1 = asinpi(0.5)
  b1 = asinpi(-0.5)
  c1 = asinpi(0.5)
  d1 = asinpi(-0.5)
  if (abs(a1 - 1.0 / 6) > e1) stop 45
  if (abs(b1 + 1.0 / 6) > e2) stop 46
  if (abs(c1 - 1.0 / 6) > e3) stop 47
  if (abs(d1 + 1.0 / 6) > e4) stop 48

  a1 = atanpi(1.0)
  b1 = atanpi(-1.0)
  c1 = atanpi(1.0)
  d1 = atanpi(-1.0)
  if (abs(a1 - 0.25) > e1) stop 49
  if (abs(b1 + 0.25) > e2) stop 50
  if (abs(c1 - 0.25) > e3) stop 51
  if (abs(d1 + 0.25) > e4) stop 52

  a1 = atan2pi(1.0, 1.0)
  b1 = atan2pi(1.0, 1.0)
  c1 = atan2pi(1.0, 1.0)
  d1 = atan2pi(1.0, 1.0)
  if (abs(a1 - 0.25) > e1) stop 53
  if (abs(b1 - 0.25) > e2) stop 54
  if (abs(c1 - 0.25) > e3) stop 55
  if (abs(d1 - 0.25) > e4) stop 56

  a1 = cospi(1._4 / 3)
  b1 = cospi(-1._8 / 3)
  c1 = cospi(4._ep / 3)
  d1 = cospi(-4._16 / 3)
  if (abs(a1 - 0.5) > e1) stop 57
  if (abs(b1 - 0.5) > e2) stop 58
  if (abs(c1 + 0.5) > e3) stop 59
  if (abs(d1 + 0.5) > e4) stop 60

  a1 = sinpi(1._4 / 6)
  b1 = sinpi(-1._8 / 6)
  c1 = sinpi(5._ep / 6)
  d1 = sinpi(-7._16 / 6)
  if (abs(a1 - 0.5) > e1) stop 61
  if (abs(b1 + 0.5) > e2) stop 62
  if (abs(c1 - 0.5) > e3) stop 63
  if (abs(d1 - 0.5) > e4) stop 64

  a1 = tanpi(0.25)
  b1 = tanpi(-0.25)
  c1 = tanpi(1.25)
  d1 = tanpi(-1.25)
  if (abs(a1 - 1.0) > e1) stop 65
  if (abs(b1 + 1.0) > e2) stop 66
  if (abs(c1 - 1.0) > e3) stop 67
  if (abs(d1 + 1.0) > e4) stop 68
end program p
