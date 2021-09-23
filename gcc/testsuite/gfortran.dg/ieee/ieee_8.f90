! { dg-do run }
! { dg-skip-if "PR libfortran/78314" { aarch64*-*-gnu* arm*-*-gnueabi arm*-*-gnueabihf } }

module foo
  use :: ieee_exceptions
  use :: ieee_arithmetic
end module foo

module bar
  use foo
  use :: ieee_arithmetic, yyy => ieee_support_rounding
  use :: ieee_arithmetic, zzz => ieee_selected_real_kind
end module

program test
  use :: bar
  use :: ieee_arithmetic, xxx => ieee_support_rounding
  implicit none

  ! IEEE functions allowed in constant expressions

  integer, parameter :: n1 = ieee_selected_real_kind(0, 0)
  logical, parameter :: l1 = ieee_support_halting(ieee_overflow)
  logical, parameter :: l2 = ieee_support_flag(ieee_overflow)
  logical, parameter :: l3 = ieee_support_flag(ieee_overflow, 0.)
  logical, parameter :: l4 = ieee_support_rounding(ieee_to_zero)
  logical, parameter :: l5 = ieee_support_rounding(ieee_to_zero, 0.d0)

  logical, parameter :: l6 = xxx(ieee_to_zero, 0.d0)
  logical, parameter :: l7 = yyy(ieee_to_zero, 0.d0)
  integer, parameter :: n2 = zzz(0, 0)

  call gee(8, ieee_to_zero, ieee_overflow)

end

! IEEE functions allowed in specification expressions

subroutine gee(n, rounding, flag)
  use :: bar
  implicit none

  integer :: n
  type(ieee_round_type) :: rounding
  type(ieee_flag_type) :: flag

  character(len=ieee_selected_real_kind(n)) :: s1
  character(len=ieee_selected_real_kind(n,2*n)) :: s2
  character(len=ieee_selected_real_kind(n,2*n,2)) :: s3

  character(len=merge(4,2,ieee_support_rounding(rounding))) :: s4
  character(len=merge(4,2,ieee_support_rounding(rounding, 0.d0))) :: s5

  character(len=merge(4,2,ieee_support_flag(flag))) :: s6
  character(len=merge(4,2,ieee_support_flag(flag, 0.))) :: s7

  character(len=merge(4,2,ieee_support_halting(flag))) :: s8

  character(len=merge(4,2,ieee_support_datatype())) :: s9
  character(len=merge(4,2,ieee_support_datatype(0.))) :: s10

  character(len=merge(4,2,ieee_support_denormal())) :: s11
  character(len=merge(4,2,ieee_support_denormal(0.))) :: s12

  character(len=merge(4,2,ieee_support_divide())) :: s13
  character(len=merge(4,2,ieee_support_divide(0.))) :: s14

  character(len=merge(4,2,ieee_support_inf())) :: s15
  character(len=merge(4,2,ieee_support_inf(0.))) :: s16

  character(len=merge(4,2,ieee_support_io())) :: s17
  character(len=merge(4,2,ieee_support_io(0.))) :: s18

  character(len=merge(4,2,ieee_support_nan())) :: s19
  character(len=merge(4,2,ieee_support_nan(0.))) :: s20

  character(len=merge(4,2,ieee_support_sqrt())) :: s21
  character(len=merge(4,2,ieee_support_sqrt(0.))) :: s22

  character(len=merge(4,2,ieee_support_standard())) :: s23
  character(len=merge(4,2,ieee_support_standard(0.))) :: s24

  character(len=merge(4,2,ieee_support_underflow_control())) :: s25
  character(len=merge(4,2,ieee_support_underflow_control(0.))) :: s26

  ! Now, check that runtime values match compile-time constants
  ! (for those that are allowed)

  integer, parameter :: x1 = ieee_selected_real_kind(8)
  integer, parameter :: x2 = ieee_selected_real_kind(8,2*8)
  integer, parameter :: x3 = ieee_selected_real_kind(8,2*8,2)

  integer, parameter :: x4 = merge(4,2,ieee_support_rounding(rounding))
  integer, parameter :: x5 = merge(4,2,ieee_support_rounding(rounding, 0.d0))

  integer, parameter :: x6 = merge(4,2,ieee_support_flag(ieee_overflow))
  integer, parameter :: x7 = merge(4,2,ieee_support_flag(ieee_overflow, 0.))

  integer, parameter :: x8 = merge(4,2,ieee_support_halting(ieee_overflow))

  if (len(s1) /= x1) STOP 1
  if (len(s2) /= x2) STOP 2
  if (len(s3) /= x3) STOP 3

  if (len(s4) /= x4) STOP 4
  if (len(s5) /= x5) STOP 5

  if (len(s6) /= x6) STOP 6
  if (len(s7) /= x7) STOP 7

  if (len(s8) /= x8) STOP 8

end subroutine
