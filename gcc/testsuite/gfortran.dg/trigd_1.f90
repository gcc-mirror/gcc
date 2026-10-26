! { dg-do run }
! { dg-additional-options "-O2 -std=f2023 -fall-intrinsics"
!
! PR fortran/121366 - degree trigonometric function and procedure pointers
!
! Test availability of degree trigonometric function from F2023 standard
! (cosd, sind, tand and their inverses) for procedure pointer assignment.
! Also test same for GNU extensions (dcosd, dsind, dtand and inverses)
! and for the two-argument versions (d)atan2d.

subroutine pr121366
  implicit none

  interface
     function func(x)
       real              :: func
       real, intent (in) :: x
     end function func
  end interface

  type pp
     procedure(func) ,pointer ,nopass :: f =>null()
  end type pp

  type(pp)  :: func_array(3)
  real      :: x = 30.
  real      :: y = 0.5
  intrinsic :: cosd, sind, tand, acosd, asind, atand

  func_array(1)%f => cosd
  func_array(2)%f => sind
  func_array(3)%f => tand
  if (func_array(1)%f(x) /= cosd(x)) stop 1
  if (func_array(2)%f(x) /= sind(x)) stop 2
  if (func_array(3)%f(x) /= tand(x)) stop 3

  func_array(1)%f => acosd
  func_array(2)%f => asind
  func_array(3)%f => atand
  if (abs (func_array(1)%f(y) - acosd(y)) > 1.e-5) stop 4
  if (abs (func_array(2)%f(y) - asind(y)) > 1.e-5) stop 5
  if (abs (func_array(3)%f(y) - atand(y)) > 1.e-6) stop 6
end

subroutine pr121366_2
  implicit none

  interface
     function func(x)
       real(8)              :: func
       real(8), intent (in) :: x
     end function func
  end interface

  type pp
     procedure(func) ,pointer ,nopass :: f =>null()
  end type pp

  type(pp)  :: func_array(3)
  real(8)   :: x = 30.
  real(8)   :: y = 0.5
  intrinsic :: dcosd, dsind, dtand, dacosd, dasind, datand

  func_array(1)%f => dcosd
  func_array(2)%f => dsind
  func_array(3)%f => dtand

  if (abs (func_array(1)%f(x) - cosd(x)) > 1.e-15) stop 11
  if (abs (func_array(2)%f(x) - sind(x)) > 1.e-15) stop 12
  if (abs (func_array(3)%f(x) - tand(x)) > 1.e-15) stop 13

  func_array(1)%f => dacosd
  func_array(2)%f => dasind
  func_array(3)%f => datand

  if (abs (func_array(1)%f(y) - acosd(y)) > 1.e-14) stop 14
  if (abs (func_array(2)%f(y) - asind(y)) > 1.e-14) stop 15
  if (abs (func_array(3)%f(y) - atand(y)) > 1.e-15) stop 16
end

subroutine pr121366_3
  implicit none

  interface
     function func2(x, y)
       real              :: func
       real, intent (in) :: x, y
     end function func2
  end interface

  type pp2
     procedure(func2) ,pointer ,nopass :: f =>null()
  end type pp2

  type(pp2) :: func2_array(1)
  real      :: x = 0.5, y = 1.0
  intrinsic :: atan2d
  intrinsic :: atan2 !, atan2pi

  func2_array(1)%f => atan2d

  if (abs (func2_array(1)%f(x,y) - atan2d(x,y)) > 1.e-6) stop 21
end

subroutine pr121366_4
  implicit none

  interface
     function func2(x, y)
       real(8)              :: func2
       real(8), intent (in) :: x, y
     end function func2
  end interface

  type pp2
     procedure(func2) ,pointer ,nopass :: f =>null()
  end type pp2

  type(pp2) :: func2_array(1)
  real(8)   :: x = 0.5, y = 1.0
  intrinsic :: datan2d

  func2_array(1)%f => datan2d

  if (abs (func2_array(1)%f(x,y) - atan2d(x,y)) > 1.e-15) stop 31
end

program test
  call pr121366
  call pr121366_2
  call pr121366_3
  call pr121366_4
end
