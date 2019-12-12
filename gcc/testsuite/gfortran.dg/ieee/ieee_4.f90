! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  integer, parameter :: s = kind(sx1), d = kind(dx1)
  type(ieee_round_type) :: mode

  ! Test IEEE_CLASS

  if (ieee_support_datatype(0._s)) then
    sx1 = 0.1_s
    if (ieee_class(sx1) /= ieee_positive_normal) STOP 1
    if (ieee_class(-sx1) /= ieee_negative_normal) STOP 2
    sx1 = huge(sx1)
    if (ieee_class(sx1) /= ieee_positive_normal) STOP 3
    if (ieee_class(-sx1) /= ieee_negative_normal) STOP 4
    if (ieee_class(2*sx1) /= ieee_positive_inf) STOP 5
    if (ieee_class(2*(-sx1)) /= ieee_negative_inf) STOP 6
    sx1 = tiny(sx1)
    if (ieee_class(sx1) /= ieee_positive_normal) STOP 7
    if (ieee_class(-sx1) /= ieee_negative_normal) STOP 8
    if (ieee_class(sx1 / 2) /= ieee_positive_denormal) STOP 9
    if (ieee_class((-sx1) / 2) /= ieee_negative_denormal) STOP 10
    sx1 = -1
    if (ieee_class(sqrt(sx1)) /= ieee_quiet_nan) STOP 11
    sx1 = 0
    if (ieee_class(sx1) /= ieee_positive_zero) STOP 12
    if (ieee_class(-sx1) /= ieee_negative_zero) STOP 13
  end if

  if (ieee_support_datatype(0._d)) then
    dx1 = 0.1_d
    if (ieee_class(dx1) /= ieee_positive_normal) STOP 14
    if (ieee_class(-dx1) /= ieee_negative_normal) STOP 15
    dx1 = huge(dx1)
    if (ieee_class(dx1) /= ieee_positive_normal) STOP 16
    if (ieee_class(-dx1) /= ieee_negative_normal) STOP 17
    if (ieee_class(2*dx1) /= ieee_positive_inf) STOP 18
    if (ieee_class(2*(-dx1)) /= ieee_negative_inf) STOP 19
    dx1 = tiny(dx1)
    if (ieee_class(dx1) /= ieee_positive_normal) STOP 20
    if (ieee_class(-dx1) /= ieee_negative_normal) STOP 21
    if (ieee_class(dx1 / 2) /= ieee_positive_denormal) STOP 22
    if (ieee_class((-dx1) / 2) /= ieee_negative_denormal) STOP 23
    dx1 = -1
    if (ieee_class(sqrt(dx1)) /= ieee_quiet_nan) STOP 24
    dx1 = 0
    if (ieee_class(dx1) /= ieee_positive_zero) STOP 25
    if (ieee_class(-dx1) /= ieee_negative_zero) STOP 26
  end if

  ! Test IEEE_VALUE and IEEE_UNORDERED

  if (ieee_support_datatype(0._s)) then
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(sx1)) STOP 27
    if (.not. ieee_unordered(sx1, sx1)) STOP 28
    if (.not. ieee_unordered(sx1, 0._s)) STOP 29
    if (.not. ieee_unordered(sx1, 0._d)) STOP 30
    if (.not. ieee_unordered(0._s, sx1)) STOP 31
    if (.not. ieee_unordered(0._d, sx1)) STOP 32
    if (ieee_unordered(0._s, 0._s)) STOP 33

    sx1 = ieee_value(sx1, ieee_positive_inf)
    if (ieee_is_finite(sx1)) STOP 34
    if (ieee_is_nan(sx1)) STOP 35
    if (ieee_is_negative(sx1)) STOP 36
    if (ieee_is_normal(sx1)) STOP 37

    sx1 = ieee_value(sx1, ieee_negative_inf)
    if (ieee_is_finite(sx1)) STOP 38
    if (ieee_is_nan(sx1)) STOP 39
    if (.not. ieee_is_negative(sx1)) STOP 40
    if (ieee_is_normal(sx1)) STOP 41

    sx1 = ieee_value(sx1, ieee_positive_normal)
    if (.not. ieee_is_finite(sx1)) STOP 42
    if (ieee_is_nan(sx1)) STOP 43
    if (ieee_is_negative(sx1)) STOP 44
    if (.not. ieee_is_normal(sx1)) STOP 45

    sx1 = ieee_value(sx1, ieee_negative_normal)
    if (.not. ieee_is_finite(sx1)) STOP 46
    if (ieee_is_nan(sx1)) STOP 47
    if (.not. ieee_is_negative(sx1)) STOP 48
    if (.not. ieee_is_normal(sx1)) STOP 49

    sx1 = ieee_value(sx1, ieee_positive_denormal)
    if (.not. ieee_is_finite(sx1)) STOP 50
    if (ieee_is_nan(sx1)) STOP 51
    if (ieee_is_negative(sx1)) STOP 52
    if (ieee_is_normal(sx1)) STOP 53
    if (sx1 <= 0) STOP 54
    if (sx1 >= tiny(sx1)) STOP 55

    sx1 = ieee_value(sx1, ieee_negative_denormal)
    if (.not. ieee_is_finite(sx1)) STOP 56
    if (ieee_is_nan(sx1)) STOP 57
    if (.not. ieee_is_negative(sx1)) STOP 58
    if (ieee_is_normal(sx1)) STOP 59
    if (sx1 >= 0) STOP 60
    if (sx1 <= -tiny(sx1)) STOP 61

    sx1 = ieee_value(sx1, ieee_positive_zero)
    if (.not. ieee_is_finite(sx1)) STOP 62
    if (ieee_is_nan(sx1)) STOP 63
    if (ieee_is_negative(sx1)) STOP 64
    if (.not. ieee_is_normal(sx1)) STOP 65
    if (sx1 /= 0) STOP 66

    sx1 = ieee_value(sx1, ieee_negative_zero)
    if (.not. ieee_is_finite(sx1)) STOP 67
    if (ieee_is_nan(sx1)) STOP 68
    if (.not. ieee_is_negative(sx1)) STOP 69
    if (.not. ieee_is_normal(sx1)) STOP 70
    if (sx1 /= 0) STOP 71

  end if

  if (ieee_support_datatype(0._d)) then
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(dx1)) STOP 72
    if (.not. ieee_unordered(dx1, dx1)) STOP 73
    if (.not. ieee_unordered(dx1, 0._s)) STOP 74
    if (.not. ieee_unordered(dx1, 0._d)) STOP 75
    if (.not. ieee_unordered(0._s, dx1)) STOP 76
    if (.not. ieee_unordered(0._d, dx1)) STOP 77
    if (ieee_unordered(0._d, 0._d)) STOP 78

    dx1 = ieee_value(dx1, ieee_positive_inf)
    if (ieee_is_finite(dx1)) STOP 79
    if (ieee_is_nan(dx1)) STOP 80
    if (ieee_is_negative(dx1)) STOP 81
    if (ieee_is_normal(dx1)) STOP 82

    dx1 = ieee_value(dx1, ieee_negative_inf)
    if (ieee_is_finite(dx1)) STOP 83
    if (ieee_is_nan(dx1)) STOP 84
    if (.not. ieee_is_negative(dx1)) STOP 85
    if (ieee_is_normal(dx1)) STOP 86

    dx1 = ieee_value(dx1, ieee_positive_normal)
    if (.not. ieee_is_finite(dx1)) STOP 87
    if (ieee_is_nan(dx1)) STOP 88
    if (ieee_is_negative(dx1)) STOP 89
    if (.not. ieee_is_normal(dx1)) STOP 90

    dx1 = ieee_value(dx1, ieee_negative_normal)
    if (.not. ieee_is_finite(dx1)) STOP 91
    if (ieee_is_nan(dx1)) STOP 92
    if (.not. ieee_is_negative(dx1)) STOP 93
    if (.not. ieee_is_normal(dx1)) STOP 94

    dx1 = ieee_value(dx1, ieee_positive_denormal)
    if (.not. ieee_is_finite(dx1)) STOP 95
    if (ieee_is_nan(dx1)) STOP 96
    if (ieee_is_negative(dx1)) STOP 97
    if (ieee_is_normal(dx1)) STOP 98
    if (dx1 <= 0) STOP 99
    if (dx1 >= tiny(dx1)) STOP 100

    dx1 = ieee_value(dx1, ieee_negative_denormal)
    if (.not. ieee_is_finite(dx1)) STOP 101
    if (ieee_is_nan(dx1)) STOP 102
    if (.not. ieee_is_negative(dx1)) STOP 103
    if (ieee_is_normal(dx1)) STOP 104
    if (dx1 >= 0) STOP 105
    if (dx1 <= -tiny(dx1)) STOP 106

    dx1 = ieee_value(dx1, ieee_positive_zero)
    if (.not. ieee_is_finite(dx1)) STOP 107
    if (ieee_is_nan(dx1)) STOP 108
    if (ieee_is_negative(dx1)) STOP 109
    if (.not. ieee_is_normal(dx1)) STOP 110
    if (dx1 /= 0) STOP 111

    dx1 = ieee_value(dx1, ieee_negative_zero)
    if (.not. ieee_is_finite(dx1)) STOP 112
    if (ieee_is_nan(dx1)) STOP 113
    if (.not. ieee_is_negative(dx1)) STOP 114
    if (.not. ieee_is_normal(dx1)) STOP 115
    if (dx1 /= 0) STOP 116

  end if

end
