! { dg-do run }

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_exceptions
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface check_equal
    procedure check_equal_float, check_equal_double
  end interface

  interface check_not_equal
    procedure check_not_equal_float, check_not_equal_double
  end interface

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  type(ieee_round_type) :: mode

  ! Test IEEE_COPY_SIGN
  sx1 = 1.3
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 1
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 2
  if (ieee_copy_sign(sx1, 1.) /= sx1) STOP 3
  if (ieee_copy_sign(sx1, -1.) /= -sx1) STOP 4
  sx1 = huge(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 5
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 6
  if (ieee_copy_sign(sx1, 1.) /= sx1) STOP 7
  if (ieee_copy_sign(sx1, -1.) /= -sx1) STOP 8
  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 9
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 10
  if (ieee_copy_sign(sx1, 1.) /= sx1) STOP 11
  if (ieee_copy_sign(sx1, -1.) /= -sx1) STOP 12
  sx1 = tiny(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 13
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 14
  if (ieee_copy_sign(sx1, 1.) /= sx1) STOP 15
  if (ieee_copy_sign(sx1, -1.) /= -sx1) STOP 16
  sx1 = tiny(sx1)
  sx1 = sx1 / 101
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 17
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 18
  if (ieee_copy_sign(sx1, 1.) /= sx1) STOP 19
  if (ieee_copy_sign(sx1, -1.) /= -sx1) STOP 20

  sx1 = -1.3
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 21
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 22
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) STOP 23
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) STOP 24
  sx1 = -huge(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 25
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 26
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) STOP 27
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) STOP 28
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 29
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 30
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) STOP 31
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) STOP 32
  sx1 = -tiny(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 33
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 34
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) STOP 35
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) STOP 36
  sx1 = -tiny(sx1)
  sx1 = sx1 / 101
  if (ieee_copy_sign(sx1, sx1) /= sx1) STOP 37
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) STOP 38
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) STOP 39
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) STOP 40

  if (ieee_class(ieee_copy_sign(0., -1.)) /= ieee_negative_zero) STOP 41
  if (ieee_class(ieee_copy_sign(-0., -1.)) /= ieee_negative_zero) STOP 42
  if (ieee_class(ieee_copy_sign(0., 1.)) /= ieee_positive_zero) STOP 43
  if (ieee_class(ieee_copy_sign(-0., 1.)) /= ieee_positive_zero) STOP 44

  sx1 = ieee_value(0., ieee_quiet_nan)
  if (ieee_class(ieee_copy_sign(sx1, 1.)) /= ieee_quiet_nan) STOP 45
  if (ieee_class(ieee_copy_sign(sx1, -1.)) /= ieee_quiet_nan) STOP 46

  dx1 = 1.3
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 47
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 48
  if (ieee_copy_sign(dx1, 1.) /= dx1) STOP 49
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) STOP 50
  dx1 = huge(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 51
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 52
  if (ieee_copy_sign(dx1, 1.d0) /= dx1) STOP 53
  if (ieee_copy_sign(dx1, -1.) /= -dx1) STOP 54
  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 55
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 56
  if (ieee_copy_sign(dx1, 1.) /= dx1) STOP 57
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) STOP 58
  dx1 = tiny(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 59
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 60
  if (ieee_copy_sign(dx1, 1.d0) /= dx1) STOP 61
  if (ieee_copy_sign(dx1, -1.) /= -dx1) STOP 62
  dx1 = tiny(dx1)
  dx1 = dx1 / 101
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 63
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 64
  if (ieee_copy_sign(dx1, 1.) /= dx1) STOP 65
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) STOP 66

  dx1 = -1.3d0
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 67
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 68
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) STOP 69
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) STOP 70
  dx1 = -huge(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 71
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 72
  if (ieee_copy_sign(dx1, 1.) /= abs(dx1)) STOP 73
  if (ieee_copy_sign(dx1, -1.d0) /= -abs(dx1)) STOP 74
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 75
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 76
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) STOP 77
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) STOP 78
  dx1 = -tiny(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 79
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 80
  if (ieee_copy_sign(dx1, 1.) /= abs(dx1)) STOP 81
  if (ieee_copy_sign(dx1, -1.d0) /= -abs(dx1)) STOP 82
  dx1 = -tiny(dx1)
  dx1 = dx1 / 101
  if (ieee_copy_sign(dx1, dx1) /= dx1) STOP 83
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) STOP 84
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) STOP 85
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) STOP 86

  if (ieee_class(ieee_copy_sign(0.d0, -1.)) /= ieee_negative_zero) STOP 87
  if (ieee_class(ieee_copy_sign(-0.d0, -1.)) /= ieee_negative_zero) STOP 88
  if (ieee_class(ieee_copy_sign(0.d0, 1.)) /= ieee_positive_zero) STOP 89
  if (ieee_class(ieee_copy_sign(-0.d0, 1.)) /= ieee_positive_zero) STOP 90

  dx1 = ieee_value(0.d0, ieee_quiet_nan)
  if (ieee_class(ieee_copy_sign(dx1, 1.d0)) /= ieee_quiet_nan) STOP 91
  if (ieee_class(ieee_copy_sign(dx1, -1.)) /= ieee_quiet_nan) STOP 92

  ! Test IEEE_LOGB

  if (ieee_logb(1.17) /= exponent(1.17) - 1) STOP 93
  if (ieee_logb(-1.17) /= exponent(-1.17) - 1) STOP 94
  if (ieee_logb(huge(sx1)) /= exponent(huge(sx1)) - 1) STOP 95
  if (ieee_logb(-huge(sx1)) /= exponent(-huge(sx1)) - 1) STOP 96
  if (ieee_logb(tiny(sx1)) /= exponent(tiny(sx1)) - 1) STOP 97
  if (ieee_logb(-tiny(sx1)) /= exponent(-tiny(sx1)) - 1) STOP 98

  if (ieee_class(ieee_logb(0.)) /= ieee_negative_inf) STOP 99
  if (ieee_class(ieee_logb(-0.)) /= ieee_negative_inf) STOP 100

  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_class(ieee_logb(sx1)) /= ieee_positive_inf) STOP 101
  if (ieee_class(ieee_logb(-sx1)) /= ieee_positive_inf) STOP 102

  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_logb(sx1)) /= ieee_quiet_nan) STOP 103

  if (ieee_logb(1.17d0) /= exponent(1.17d0) - 1) STOP 104
  if (ieee_logb(-1.17d0) /= exponent(-1.17d0) - 1) STOP 105
  if (ieee_logb(huge(dx1)) /= exponent(huge(dx1)) - 1) STOP 106
  if (ieee_logb(-huge(dx1)) /= exponent(-huge(dx1)) - 1) STOP 107
  if (ieee_logb(tiny(dx1)) /= exponent(tiny(dx1)) - 1) STOP 108
  if (ieee_logb(-tiny(dx1)) /= exponent(-tiny(dx1)) - 1) STOP 109

  if (ieee_class(ieee_logb(0.d0)) /= ieee_negative_inf) STOP 110
  if (ieee_class(ieee_logb(-0.d0)) /= ieee_negative_inf) STOP 111

  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_class(ieee_logb(dx1)) /= ieee_positive_inf) STOP 112
  if (ieee_class(ieee_logb(-dx1)) /= ieee_positive_inf) STOP 113

  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_logb(dx1)) /= ieee_quiet_nan) STOP 114

  ! Test IEEE_NEXT_AFTER

  if (ieee_next_after(0.12, 1.0) /= nearest(0.12, 1.0)) STOP 115
  if (ieee_next_after(0.12, -1.0) /= nearest(0.12, -1.0)) STOP 116

  sx1 = 0.12
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 117
  sx1 = -0.12
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 118
  sx1 = huge(sx1)
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 119
  sx1 = tiny(sx1)
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 120
  sx1 = 0
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 121
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_next_after(sx1, sx1) /= sx1) STOP 122
  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_next_after(sx1, sx1)) /= ieee_quiet_nan) STOP 123

  if (ieee_next_after(0., 1.0) <= 0) STOP 124
  if (ieee_next_after(0., -1.0) >= 0) STOP 125
  sx1 = ieee_next_after(huge(sx1), ieee_value(sx1, ieee_negative_inf))
  if (.not. sx1 < huge(sx1)) STOP 126
  sx1 = ieee_next_after(huge(sx1), ieee_value(sx1, ieee_positive_inf))
  if (ieee_class(sx1) /= ieee_positive_inf) STOP 127
  sx1 = ieee_next_after(-tiny(sx1), 1.0)
  if (ieee_class(sx1) /= ieee_negative_denormal) STOP 128

  if (ieee_next_after(0.12d0, 1.0d0) /= nearest(0.12d0, 1.0)) STOP 129
  if (ieee_next_after(0.12d0, -1.0) /= nearest(0.12d0, -1.0)) STOP 130

  dx1 = 0.12
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 131
  dx1 = -0.12
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 132
  dx1 = huge(dx1)
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 133
  dx1 = tiny(dx1)
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 134
  dx1 = 0
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 135
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_next_after(dx1, dx1) /= dx1) STOP 136
  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_next_after(dx1, dx1)) /= ieee_quiet_nan) STOP 137

  if (ieee_next_after(0.d0, 1.0) <= 0) STOP 138
  if (ieee_next_after(0.d0, -1.0d0) >= 0) STOP 139
  dx1 = ieee_next_after(huge(dx1), ieee_value(dx1, ieee_negative_inf))
  if (.not. dx1 < huge(dx1)) STOP 140
  dx1 = ieee_next_after(huge(dx1), ieee_value(dx1, ieee_positive_inf))
  if (ieee_class(dx1) /= ieee_positive_inf) STOP 141
  dx1 = ieee_next_after(-tiny(dx1), 1.0d0)
  if (ieee_class(dx1) /= ieee_negative_denormal) STOP 142

  ! Test IEEE_REM

  if (ieee_rem(4.0, 3.0) /= 1.0) STOP 143
  if (ieee_rem(-4.0, 3.0) /= -1.0) STOP 144
  if (ieee_rem(2.0, 3.0d0) /= -1.0d0) STOP 145
  if (ieee_rem(-2.0, 3.0d0) /= 1.0d0) STOP 146
  if (ieee_rem(2.0d0, 3.0d0) /= -1.0d0) STOP 147
  if (ieee_rem(-2.0d0, 3.0d0) /= 1.0d0) STOP 148

  if (ieee_class(ieee_rem(ieee_value(0., ieee_quiet_nan), 1.0)) &
      /= ieee_quiet_nan) STOP 149
  if (ieee_class(ieee_rem(1.0, ieee_value(0.d0, ieee_quiet_nan))) &
      /= ieee_quiet_nan) STOP 150

  if (ieee_class(ieee_rem(ieee_value(0., ieee_positive_inf), 1.0)) &
      /= ieee_quiet_nan) STOP 151
  if (ieee_class(ieee_rem(ieee_value(0.d0, ieee_negative_inf), 1.0)) &
      /= ieee_quiet_nan) STOP 152
  if (ieee_rem(-1.0, ieee_value(0., ieee_positive_inf)) &
      /= -1.0) STOP 153
  if (ieee_rem(1.0, ieee_value(0.d0, ieee_negative_inf)) &
      /= 1.0) STOP 154


  ! Test IEEE_RINT

  if (ieee_support_rounding (ieee_nearest, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_nearest)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) STOP 155
  end if

  if (ieee_support_rounding (ieee_up, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_up)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 3) STOP 156
  end if

  if (ieee_support_rounding (ieee_down, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_down)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) STOP 157
  end if

  if (ieee_support_rounding (ieee_to_zero, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_to_zero)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) STOP 158
  end if

  if (ieee_class(ieee_rint(0.)) /= ieee_positive_zero) STOP 159
  if (ieee_class(ieee_rint(-0.)) /= ieee_negative_zero) STOP 160

  if (ieee_support_rounding (ieee_nearest, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_nearest)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) STOP 161
  end if

  if (ieee_support_rounding (ieee_up, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_up)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 3) STOP 162
  end if

  if (ieee_support_rounding (ieee_down, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_down)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) STOP 163
  end if

  if (ieee_support_rounding (ieee_to_zero, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_to_zero)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) STOP 164
  end if

  if (ieee_class(ieee_rint(0.d0)) /= ieee_positive_zero) STOP 165
  if (ieee_class(ieee_rint(-0.d0)) /= ieee_negative_zero) STOP 166

  ! Test IEEE_SCALB

  sx1 = 1
  if (ieee_scalb(sx1, 2) /= 4.) STOP 167
  if (ieee_scalb(-sx1, 2) /= -4.) STOP 168
  if (ieee_scalb(sx1, -2) /= 1/4.) STOP 169
  if (ieee_scalb(-sx1, -2) /= -1/4.) STOP 170
  if (ieee_class(ieee_scalb(sx1, huge(0))) /= ieee_positive_inf) STOP 171
  if (ieee_class(ieee_scalb(-sx1, huge(0))) /= ieee_negative_inf) STOP 172
  if (ieee_class(ieee_scalb(sx1, -huge(0))) /= ieee_positive_zero) STOP 173
  if (ieee_class(ieee_scalb(-sx1, -huge(0))) /= ieee_negative_zero) STOP 174

  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_scalb(sx1, 1)) /= ieee_quiet_nan) STOP 175
  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_class(ieee_scalb(sx1, -42)) /= ieee_positive_inf) STOP 176
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_class(ieee_scalb(sx1, -42)) /= ieee_negative_inf) STOP 177

  dx1 = 1
  if (ieee_scalb(dx1, 2) /= 4.d0) STOP 178
  if (ieee_scalb(-dx1, 2) /= -4.d0) STOP 179
  if (ieee_scalb(dx1, -2) /= 1/4.d0) STOP 180
  if (ieee_scalb(-dx1, -2) /= -1/4.d0) STOP 181
  if (ieee_class(ieee_scalb(dx1, huge(0))) /= ieee_positive_inf) STOP 182
  if (ieee_class(ieee_scalb(-dx1, huge(0))) /= ieee_negative_inf) STOP 183
  if (ieee_class(ieee_scalb(dx1, -huge(0))) /= ieee_positive_zero) STOP 184
  if (ieee_class(ieee_scalb(-dx1, -huge(0))) /= ieee_negative_zero) STOP 185

  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_scalb(dx1, 1)) /= ieee_quiet_nan) STOP 186
  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_class(ieee_scalb(dx1, -42)) /= ieee_positive_inf) STOP 187
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_class(ieee_scalb(dx1, -42)) /= ieee_negative_inf) STOP 188

contains

  subroutine check_equal_float (x, y)
    real, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      STOP 189
    end if
  end subroutine

  subroutine check_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      STOP 190
    end if
  end subroutine

  subroutine check_not_equal_float (x, y)
    real, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      STOP 191
    end if
  end subroutine

  subroutine check_not_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      STOP 192
    end if
  end subroutine

end
