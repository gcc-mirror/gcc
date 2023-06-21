! { dg-do run }
!
program test
  call real()
  call double()
  call large1()
  call large2()
end program test


subroutine real
  use ieee_arithmetic
  implicit none

  real :: inf, nan

  inf = ieee_value(inf, ieee_positive_inf)
  nan = ieee_value(nan, ieee_quiet_nan)

  if (ieee_min_num (0., 0.) /= 0.) stop 1
  if (ieee_min_num (-0., -0.) /= -0.) stop 2
  if (.not. ieee_signbit (ieee_min_num (-0., -0.))) stop 3
  if (ieee_min_num (0., -0.) /= -0.) stop 4
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (0., -0.))) stop 5
  if (ieee_min_num (-0., 0.) /= 0.) stop 6
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (-0., 0.))) stop 7

  if (ieee_min_num (9., 0.) /= 0.) stop 8
  if (ieee_min_num (0., 9.) /= 0.) stop 9
  if (ieee_min_num (-9., 0.) /= -9.) stop 10
  if (ieee_min_num (0., -9.) /= -9.) stop 11

  if (ieee_min_num (inf, 9.) /= 9.) stop 12
  if (ieee_min_num (0., inf) /= 0.) stop 13
  if (ieee_min_num (-9., inf) /= -9.) stop 14
  if (ieee_min_num (inf, -9.) /= -9.) stop 15
  if (ieee_min_num (-inf, 9.) /= -inf) stop 16
  if (ieee_min_num (0., -inf) /= -inf) stop 17
  if (ieee_min_num (-9., -inf) /= -inf) stop 18
  if (ieee_min_num (-inf, -9.) /= -inf) stop 19

  if (ieee_min_num (0., nan) /= 0.) stop 20
  if (ieee_min_num (nan, 0.) /= 0.) stop 21
  if (ieee_min_num (-0., nan) /= -0.) stop 22
  if (.not. ieee_signbit (ieee_min_num (-0., nan))) stop 23
  if (ieee_min_num (nan, -0.) /= -0.) stop 24
  if (.not. ieee_signbit (ieee_min_num (nan, -0.))) stop 25
  if (ieee_min_num (9., nan) /= 9.) stop 26
  if (ieee_min_num (nan, 9.) /= 9.) stop 27
  if (ieee_min_num (-9., nan) /= -9.) stop 28
  if (ieee_min_num (nan, -9.) /= -9.) stop 29

  if (ieee_min_num (nan, inf) /= inf) stop 30
  if (ieee_min_num (inf, nan) /= inf) stop 31
  if (ieee_min_num (nan, -inf) /= -inf) stop 32
  if (ieee_min_num (-inf, nan) /= -inf) stop 33

  if (.not. ieee_is_nan (ieee_min_num (nan, nan))) stop 34
end subroutine real


subroutine double
  use ieee_arithmetic
  implicit none

  double precision :: inf, nan

  inf = ieee_value(inf, ieee_positive_inf)
  nan = ieee_value(nan, ieee_quiet_nan)

  if (ieee_min_num (0.d0, 0.d0) /= 0.d0) stop 35
  if (ieee_min_num (-0.d0, -0.d0) /= -0.d0) stop 36
  if (.not. ieee_signbit (ieee_min_num (-0.d0, -0.d0))) stop 37
  if (ieee_min_num (0.d0, -0.d0) /= 0.d0) stop 38
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (0.d0, -0.d0))) stop 39
  if (ieee_min_num (-0.d0, 0.d0) /= 0.d0) stop 40
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (-0.d0, 0.d0))) stop 41

  if (ieee_min_num (9.d0, 0.d0) /= 0.d0) stop 42
  if (ieee_min_num (0.d0, 9.d0) /= 0.d0) stop 43
  if (ieee_min_num (-9.d0, 0.d0) /= -9.d0) stop 44
  if (ieee_min_num (0.d0, -9.d0) /= -9.d0) stop 45

  if (ieee_min_num (inf, 9.d0) /= 9.d0) stop 46
  if (ieee_min_num (0.d0, inf) /= 0.d0) stop 47
  if (ieee_min_num (-9.d0, inf) /= -9.d0) stop 48
  if (ieee_min_num (inf, -9.d0) /= -9.d0) stop 49
  if (ieee_min_num (-inf, 9.d0) /= -inf) stop 50
  if (ieee_min_num (0.d0, -inf) /= -inf) stop 51
  if (ieee_min_num (-9.d0, -inf) /= -inf) stop 52
  if (ieee_min_num (-inf, -9.d0) /= -inf) stop 53

  if (ieee_min_num (0.d0, nan) /= 0.d0) stop 54
  if (ieee_min_num (nan, 0.d0) /= 0.d0) stop 55
  if (ieee_min_num (-0.d0, nan) /= -0.d0) stop 56
  if (.not. ieee_signbit (ieee_min_num (-0.d0, nan))) stop 57
  if (ieee_min_num (nan, -0.d0) /= -0.d0) stop 58
  if (.not. ieee_signbit (ieee_min_num (nan, -0.d0))) stop 59
  if (ieee_min_num (9.d0, nan) /= 9.d0) stop 60
  if (ieee_min_num (nan, 9.d0) /= 9.d0) stop 61
  if (ieee_min_num (-9.d0, nan) /= -9.d0) stop 62
  if (ieee_min_num (nan, -9.d0) /= -9.d0) stop 63

  if (ieee_min_num (nan, inf) /= inf) stop 64
  if (ieee_min_num (inf, nan) /= inf) stop 65
  if (ieee_min_num (nan, -inf) /= -inf) stop 66
  if (ieee_min_num (-inf, nan) /= -inf) stop 67

  if (.not. ieee_is_nan (ieee_min_num (nan, nan))) stop 68
end subroutine double


subroutine large1
  use ieee_arithmetic
  implicit none

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k1) :: inf, nan

  inf = ieee_value(inf, ieee_positive_inf)
  nan = ieee_value(nan, ieee_quiet_nan)

  if (ieee_min_num (0._k1, 0._k1) /= 0._k1) stop 35
  if (ieee_min_num (-0._k1, -0._k1) /= -0._k1) stop 36
  if (.not. ieee_signbit (ieee_min_num (-0._k1, -0._k1))) stop 37
  if (ieee_min_num (0._k1, -0._k1) /= 0._k1) stop 38
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (0._k1, -0._k1))) stop 39
  if (ieee_min_num (-0._k1, 0._k1) /= 0._k1) stop 40
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (-0._k1, 0._k1))) stop 41

  if (ieee_min_num (9._k1, 0._k1) /= 0._k1) stop 42
  if (ieee_min_num (0._k1, 9._k1) /= 0._k1) stop 43
  if (ieee_min_num (-9._k1, 0._k1) /= -9._k1) stop 44
  if (ieee_min_num (0._k1, -9._k1) /= -9._k1) stop 45

  if (ieee_min_num (inf, 9._k1) /= 9._k1) stop 46
  if (ieee_min_num (0._k1, inf) /= 0._k1) stop 47
  if (ieee_min_num (-9._k1, inf) /= -9._k1) stop 48
  if (ieee_min_num (inf, -9._k1) /= -9._k1) stop 49
  if (ieee_min_num (-inf, 9._k1) /= -inf) stop 50
  if (ieee_min_num (0._k1, -inf) /= -inf) stop 51
  if (ieee_min_num (-9._k1, -inf) /= -inf) stop 52
  if (ieee_min_num (-inf, -9._k1) /= -inf) stop 53

  if (ieee_min_num (0._k1, nan) /= 0._k1) stop 54
  if (ieee_min_num (nan, 0._k1) /= 0._k1) stop 55
  if (ieee_min_num (-0._k1, nan) /= -0._k1) stop 56
  if (.not. ieee_signbit (ieee_min_num (-0._k1, nan))) stop 57
  if (ieee_min_num (nan, -0._k1) /= -0._k1) stop 58
  if (.not. ieee_signbit (ieee_min_num (nan, -0._k1))) stop 59
  if (ieee_min_num (9._k1, nan) /= 9._k1) stop 60
  if (ieee_min_num (nan, 9._k1) /= 9._k1) stop 61
  if (ieee_min_num (-9._k1, nan) /= -9._k1) stop 62
  if (ieee_min_num (nan, -9._k1) /= -9._k1) stop 63

  if (ieee_min_num (nan, inf) /= inf) stop 64
  if (ieee_min_num (inf, nan) /= inf) stop 65
  if (ieee_min_num (nan, -inf) /= -inf) stop 66
  if (ieee_min_num (-inf, nan) /= -inf) stop 67

  if (.not. ieee_is_nan (ieee_min_num (nan, nan))) stop 68
end subroutine large1


subroutine large2
  use ieee_arithmetic
  implicit none

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k2) :: inf, nan

  inf = ieee_value(inf, ieee_positive_inf)
  nan = ieee_value(nan, ieee_quiet_nan)

  if (ieee_min_num (0._k2, 0._k2) /= 0._k2) stop 35
  if (ieee_min_num (-0._k2, -0._k2) /= -0._k2) stop 36
  if (.not. ieee_signbit (ieee_min_num (-0._k2, -0._k2))) stop 37
  if (ieee_min_num (0._k2, -0._k2) /= 0._k2) stop 38
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (0._k2, -0._k2))) stop 39
  if (ieee_min_num (-0._k2, 0._k2) /= 0._k2) stop 40
  ! Processor-dependent
  !if (ieee_signbit (ieee_min_num (-0._k2, 0._k2))) stop 41

  if (ieee_min_num (9._k2, 0._k2) /= 0._k2) stop 42
  if (ieee_min_num (0._k2, 9._k2) /= 0._k2) stop 43
  if (ieee_min_num (-9._k2, 0._k2) /= -9._k2) stop 44
  if (ieee_min_num (0._k2, -9._k2) /= -9._k2) stop 45

  if (ieee_min_num (inf, 9._k2) /= 9._k2) stop 46
  if (ieee_min_num (0._k2, inf) /= 0._k2) stop 47
  if (ieee_min_num (-9._k2, inf) /= -9._k2) stop 48
  if (ieee_min_num (inf, -9._k2) /= -9._k2) stop 49
  if (ieee_min_num (-inf, 9._k2) /= -inf) stop 50
  if (ieee_min_num (0._k2, -inf) /= -inf) stop 51
  if (ieee_min_num (-9._k2, -inf) /= -inf) stop 52
  if (ieee_min_num (-inf, -9._k2) /= -inf) stop 53

  if (ieee_min_num (0._k2, nan) /= 0._k2) stop 54
  if (ieee_min_num (nan, 0._k2) /= 0._k2) stop 55
  if (ieee_min_num (-0._k2, nan) /= -0._k2) stop 56
  if (.not. ieee_signbit (ieee_min_num (-0._k2, nan))) stop 57
  if (ieee_min_num (nan, -0._k2) /= -0._k2) stop 58
  if (.not. ieee_signbit (ieee_min_num (nan, -0._k2))) stop 59
  if (ieee_min_num (9._k2, nan) /= 9._k2) stop 60
  if (ieee_min_num (nan, 9._k2) /= 9._k2) stop 61
  if (ieee_min_num (-9._k2, nan) /= -9._k2) stop 62
  if (ieee_min_num (nan, -9._k2) /= -9._k2) stop 63

  if (ieee_min_num (nan, inf) /= inf) stop 64
  if (ieee_min_num (inf, nan) /= inf) stop 65
  if (ieee_min_num (nan, -inf) /= -inf) stop 66
  if (ieee_min_num (-inf, nan) /= -inf) stop 67

  if (.not. ieee_is_nan (ieee_min_num (nan, nan))) stop 68
end subroutine large2

