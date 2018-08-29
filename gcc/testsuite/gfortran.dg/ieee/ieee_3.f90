! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  integer, parameter :: s = kind(sx1), d = kind(dx1)
  type(ieee_round_type) :: mode

  ! Test IEEE_IS_FINITE

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_finite(0.2_s)) STOP 1
    if (.not. ieee_is_finite(-0.2_s)) STOP 2
    if (.not. ieee_is_finite(0._s)) STOP 3
    if (.not. ieee_is_finite(-0._s)) STOP 4
    if (.not. ieee_is_finite(tiny(0._s))) STOP 5
    if (.not. ieee_is_finite(tiny(0._s)/100)) STOP 6
    if (.not. ieee_is_finite(huge(0._s))) STOP 7
    if (.not. ieee_is_finite(-huge(0._s))) STOP 8
    sx1 = huge(sx1)
    if (ieee_is_finite(2*sx1)) STOP 9
    if (ieee_is_finite(2*(-sx1))) STOP 10
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_finite(sx1)) STOP 11
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_finite(0.2_d)) STOP 12
    if (.not. ieee_is_finite(-0.2_d)) STOP 13
    if (.not. ieee_is_finite(0._d)) STOP 14
    if (.not. ieee_is_finite(-0._d)) STOP 15
    if (.not. ieee_is_finite(tiny(0._d))) STOP 16
    if (.not. ieee_is_finite(tiny(0._d)/100)) STOP 17
    if (.not. ieee_is_finite(huge(0._d))) STOP 18
    if (.not. ieee_is_finite(-huge(0._d))) STOP 19
    dx1 = huge(dx1)
    if (ieee_is_finite(2*dx1)) STOP 20
    if (ieee_is_finite(2*(-dx1))) STOP 21
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_finite(dx1)) STOP 22
  end if

  ! Test IEEE_IS_NAN

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_nan(0.2_s)) STOP 23
    if (ieee_is_nan(-0.2_s)) STOP 24
    if (ieee_is_nan(0._s)) STOP 25
    if (ieee_is_nan(-0._s)) STOP 26
    if (ieee_is_nan(tiny(0._s))) STOP 27
    if (ieee_is_nan(tiny(0._s)/100)) STOP 28
    if (ieee_is_nan(huge(0._s))) STOP 29
    if (ieee_is_nan(-huge(0._s))) STOP 30
    sx1 = huge(sx1)
    if (ieee_is_nan(2*sx1)) STOP 31
    if (ieee_is_nan(2*(-sx1))) STOP 32
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(sx1)) STOP 33
    sx1 = -1
    if (.not. ieee_is_nan(sqrt(sx1))) STOP 34
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_nan(0.2_d)) STOP 35
    if (ieee_is_nan(-0.2_d)) STOP 36
    if (ieee_is_nan(0._d)) STOP 37
    if (ieee_is_nan(-0._d)) STOP 38
    if (ieee_is_nan(tiny(0._d))) STOP 39
    if (ieee_is_nan(tiny(0._d)/100)) STOP 40
    if (ieee_is_nan(huge(0._d))) STOP 41
    if (ieee_is_nan(-huge(0._d))) STOP 42
    dx1 = huge(dx1)
    if (ieee_is_nan(2*dx1)) STOP 43
    if (ieee_is_nan(2*(-dx1))) STOP 44
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(dx1)) STOP 45
    dx1 = -1
    if (.not. ieee_is_nan(sqrt(dx1))) STOP 46
  end if

  ! IEEE_IS_NEGATIVE

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_negative(0.2_s)) STOP 47
    if (.not. ieee_is_negative(-0.2_s)) STOP 48
    if (ieee_is_negative(0._s)) STOP 49
    if (.not. ieee_is_negative(-0._s)) STOP 50
    if (ieee_is_negative(tiny(0._s))) STOP 51
    if (ieee_is_negative(tiny(0._s)/100)) STOP 52
    if (.not. ieee_is_negative(-tiny(0._s))) STOP 53
    if (.not. ieee_is_negative(-tiny(0._s)/100)) STOP 54
    if (ieee_is_negative(huge(0._s))) STOP 55
    if (.not. ieee_is_negative(-huge(0._s))) STOP 56
    sx1 = huge(sx1)
    if (ieee_is_negative(2*sx1)) STOP 57
    if (.not. ieee_is_negative(2*(-sx1))) STOP 58
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_negative(sx1)) STOP 59
    sx1 = -1
    if (ieee_is_negative(sqrt(sx1))) STOP 60
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_negative(0.2_d)) STOP 61
    if (.not. ieee_is_negative(-0.2_d)) STOP 62
    if (ieee_is_negative(0._d)) STOP 63
    if (.not. ieee_is_negative(-0._d)) STOP 64
    if (ieee_is_negative(tiny(0._d))) STOP 65
    if (ieee_is_negative(tiny(0._d)/100)) STOP 66
    if (.not. ieee_is_negative(-tiny(0._d))) STOP 67
    if (.not. ieee_is_negative(-tiny(0._d)/100)) STOP 68
    if (ieee_is_negative(huge(0._d))) STOP 69
    if (.not. ieee_is_negative(-huge(0._d))) STOP 70
    dx1 = huge(dx1)
    if (ieee_is_negative(2*dx1)) STOP 71
    if (.not. ieee_is_negative(2*(-dx1))) STOP 72
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_negative(dx1)) STOP 73
    dx1 = -1
    if (ieee_is_negative(sqrt(dx1))) STOP 74
  end if

  ! Test IEEE_IS_NORMAL

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_normal(0.2_s)) STOP 75
    if (.not. ieee_is_normal(-0.2_s)) STOP 76
    if (.not. ieee_is_normal(0._s)) STOP 77
    if (.not. ieee_is_normal(-0._s)) STOP 78
    if (.not. ieee_is_normal(tiny(0._s))) STOP 79
    if (ieee_is_normal(tiny(0._s)/100)) STOP 80
    if (.not. ieee_is_normal(-tiny(0._s))) STOP 81
    if (ieee_is_normal(-tiny(0._s)/100)) STOP 82
    if (.not. ieee_is_normal(huge(0._s))) STOP 83
    if (.not. ieee_is_normal(-huge(0._s))) STOP 84
    sx1 = huge(sx1)
    if (ieee_is_normal(2*sx1)) STOP 85
    if (ieee_is_normal(2*(-sx1))) STOP 86
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_normal(sx1)) STOP 87
    sx1 = -1
    if (ieee_is_normal(sqrt(sx1))) STOP 88
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_normal(0.2_d)) STOP 89
    if (.not. ieee_is_normal(-0.2_d)) STOP 90
    if (.not. ieee_is_normal(0._d)) STOP 91
    if (.not. ieee_is_normal(-0._d)) STOP 92
    if (.not. ieee_is_normal(tiny(0._d))) STOP 93
    if (ieee_is_normal(tiny(0._d)/100)) STOP 94
    if (.not. ieee_is_normal(-tiny(0._d))) STOP 95
    if (ieee_is_normal(-tiny(0._d)/100)) STOP 96
    if (.not. ieee_is_normal(huge(0._d))) STOP 97
    if (.not. ieee_is_normal(-huge(0._d))) STOP 98
    dx1 = huge(dx1)
    if (ieee_is_normal(2*dx1)) STOP 99
    if (ieee_is_normal(2*(-dx1))) STOP 100
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_normal(dx1)) STOP 101
    dx1 = -1
    if (ieee_is_normal(sqrt(dx1))) STOP 102
  end if

end
