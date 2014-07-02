! { dg-do run }
! { dg-additional-options "-mfp-rounding-mode=d" { target alpha*-*-* } }

  use, intrinsic :: ieee_features, only : ieee_rounding
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface check_equal
    procedure check_equal_float, check_equal_double
  end interface

  interface check_not_equal
    procedure check_not_equal_float, check_not_equal_double
  end interface

  interface divide
    procedure divide_float, divide_double
  end interface

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  type(ieee_round_type) :: mode

  ! We should support at least C float and C double types
  if (ieee_support_rounding(ieee_nearest)) then
    if (.not. ieee_support_rounding(ieee_nearest, 0.)) call abort
    if (.not. ieee_support_rounding(ieee_nearest, 0.d0)) call abort
  end if

  ! The initial rounding mode should probably be NEAREST
  ! (at least on the platforms we currently support)
  if (ieee_support_rounding(ieee_nearest, 0.)) then
    call ieee_get_rounding_mode (mode)
    if (mode /= ieee_nearest) call abort
  end if


  if (ieee_support_rounding(ieee_up, sx1) .and. &
      ieee_support_rounding(ieee_down, sx1) .and. &
      ieee_support_rounding(ieee_nearest, sx1) .and. &
      ieee_support_rounding(ieee_to_zero, sx1)) then

    sx1 = 1
    sx2 = 3
    sx1 = divide(sx1, sx2, ieee_up)

    sx3 = 1
    sx2 = 3
    sx3 = divide(sx3, sx2, ieee_down)
    call check_not_equal(sx1, sx3)
    call check_equal(sx3, nearest(sx1, -1.))
    call check_equal(sx1, nearest(sx3,  1.))

    call check_equal(1./3., divide(1., 3., ieee_nearest))
    call check_equal(-1./3., divide(-1., 3., ieee_nearest))

    call check_equal(divide(3., 7., ieee_to_zero), &
                    divide(3., 7., ieee_down))
    call check_equal(divide(-3., 7., ieee_to_zero), &
                    divide(-3., 7., ieee_up))

  end if

  if (ieee_support_rounding(ieee_up, dx1) .and. &
      ieee_support_rounding(ieee_down, dx1) .and. &
      ieee_support_rounding(ieee_nearest, dx1) .and. &
      ieee_support_rounding(ieee_to_zero, dx1)) then

    dx1 = 1
    dx2 = 3
    dx1 = divide(dx1, dx2, ieee_up)

    dx3 = 1
    dx2 = 3
    dx3 = divide(dx3, dx2, ieee_down)
    call check_not_equal(dx1, dx3)
    call check_equal(dx3, nearest(dx1, -1.d0))
    call check_equal(dx1, nearest(dx3,  1.d0))

    call check_equal(1.d0/3.d0, divide(1.d0, 3.d0, ieee_nearest))
    call check_equal(-1.d0/3.d0, divide(-1.d0, 3.d0, ieee_nearest))

    call check_equal(divide(3.d0, 7.d0, ieee_to_zero), &
                    divide(3.d0, 7.d0, ieee_down))
    call check_equal(divide(-3.d0, 7.d0, ieee_to_zero), &
                    divide(-3.d0, 7.d0, ieee_up))

  end if

contains

  real function divide_float (x, y, rounding) result(res)
    use, intrinsic :: ieee_arithmetic
    real, intent(in) :: x, y
    type(ieee_round_type), intent(in) :: rounding
    type(ieee_round_type) :: old

    call ieee_get_rounding_mode (old)
    call ieee_set_rounding_mode (rounding)

    res = x / y

    call ieee_set_rounding_mode (old)
  end function

  double precision function divide_double (x, y, rounding) result(res)
    use, intrinsic :: ieee_arithmetic
    double precision, intent(in) :: x, y
    type(ieee_round_type), intent(in) :: rounding
    type(ieee_round_type) :: old

    call ieee_get_rounding_mode (old)
    call ieee_set_rounding_mode (rounding)

    res = x / y

    call ieee_set_rounding_mode (old)
  end function

  subroutine check_equal_float (x, y)
    real, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal_float (x, y)
    real, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

end
