! { dg-do run }
! { dg-additional-options "-mfp-rounding-mode=d" { target alpha*-*-* } }

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  interface check_equal
    procedure check_equal1, check_equal2
  end interface

  interface check_not_equal
    procedure check_not_equal1, check_not_equal2
  end interface

  interface divide
    procedure divide1, divide2
  end interface

  real(kind=k1) :: x1, x2, x3
  real(kind=k2) :: y1, y2, y3
  type(ieee_round_type) :: mode

  if (ieee_support_rounding(ieee_up, x1) .and. &
      ieee_support_rounding(ieee_down, x1) .and. &
      ieee_support_rounding(ieee_nearest, x1) .and. &
      ieee_support_rounding(ieee_to_zero, x1)) then

    x1 = 1
    x2 = 3
    x1 = divide(x1, x2, ieee_up)

    x3 = 1
    x2 = 3
    x3 = divide(x3, x2, ieee_down)
    call check_not_equal(x1, x3)
    call check_equal(x3, nearest(x1, -1._k1))
    call check_equal(x1, nearest(x3,  1._k1))

    call check_equal(1._k1/3._k1, divide(1._k1, 3._k1, ieee_nearest))
    call check_equal(-1._k1/3._k1, divide(-1._k1, 3._k1, ieee_nearest))

    call check_equal(divide(3._k1, 7._k1, ieee_to_zero), &
                    divide(3._k1, 7._k1, ieee_down))
    call check_equal(divide(-3._k1, 7._k1, ieee_to_zero), &
                    divide(-3._k1, 7._k1, ieee_up))

  end if

  if (ieee_support_rounding(ieee_up, y1) .and. &
      ieee_support_rounding(ieee_down, y1) .and. &
      ieee_support_rounding(ieee_nearest, y1) .and. &
      ieee_support_rounding(ieee_to_zero, y1)) then

    y1 = 1
    y2 = 3
    y1 = divide(y1, y2, ieee_up)

    y3 = 1
    y2 = 3
    y3 = divide(y3, y2, ieee_down)
    call check_not_equal(y1, y3)
    call check_equal(y3, nearest(y1, -1._k2))
    call check_equal(y1, nearest(y3,  1._k2))

    call check_equal(1._k2/3._k2, divide(1._k2, 3._k2, ieee_nearest))
    call check_equal(-1._k2/3._k2, divide(-1._k2, 3._k2, ieee_nearest))

    call check_equal(divide(3._k2, 7._k2, ieee_to_zero), &
                    divide(3._k2, 7._k2, ieee_down))
    call check_equal(divide(-3._k2, 7._k2, ieee_to_zero), &
                    divide(-3._k2, 7._k2, ieee_up))

  end if

contains

  real(kind=k1) function divide1 (x, y, rounding) result(res)
    use, intrinsic :: ieee_arithmetic
    real(kind=k1), intent(in) :: x, y
    type(ieee_round_type), intent(in) :: rounding
    type(ieee_round_type) :: old

    call ieee_get_rounding_mode (old)
    call ieee_set_rounding_mode (rounding)

    res = x / y

    call ieee_set_rounding_mode (old)
  end function

  real(kind=k2) function divide2 (x, y, rounding) result(res)
    use, intrinsic :: ieee_arithmetic
    real(kind=k2), intent(in) :: x, y
    type(ieee_round_type), intent(in) :: rounding
    type(ieee_round_type) :: old

    call ieee_get_rounding_mode (old)
    call ieee_set_rounding_mode (rounding)

    res = x / y

    call ieee_set_rounding_mode (old)
  end function

  subroutine check_equal1 (x, y)
    real(kind=k1), intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_equal2 (x, y)
    real(kind=k2), intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal1 (x, y)
    real(kind=k1), intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal2 (x, y)
    real(kind=k2), intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

end
