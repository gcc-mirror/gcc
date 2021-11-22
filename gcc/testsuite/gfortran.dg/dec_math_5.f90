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
end program p
