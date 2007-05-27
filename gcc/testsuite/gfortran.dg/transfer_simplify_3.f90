! { dg-do run }
! { dg-options "-fno-range-check" }
! PR fortran/32083
!
! Test transfers of +Inf and -Inf
! Testcase contributed by Jos de Kloe <kloedej@knmi.nl>
!

PROGRAM TestInfinite
  IMPLICIT NONE
  integer, parameter :: i8_ = Selected_Int_Kind(18)  ! = integer*8
  integer, parameter :: r8_ = Selected_Real_Kind(15,307)  ! = real*8

  integer(i8_), parameter :: bit_pattern_PosInf_i8_p = 9218868437227405312_i8_
  integer(i8_), parameter :: bit_pattern_NegInf_i8_p = -4503599627370496_i8_

  integer(i8_) :: bit_pattern_PosInf_i8 = 9218868437227405312_i8_
  integer(i8_) :: bit_pattern_NegInf_i8 = -4503599627370496_i8_

  integer(i8_) :: bit_pattern_PosInf_i8_hex
  integer(i8_) :: bit_pattern_NegInf_i8_hex

  integer(i8_) :: i
  real(r8_)    :: r

  data bit_pattern_PosInf_i8_hex /z'7FF0000000000000'/
  !data bit_pattern_NegInf_i8_hex /z'FFF0000000000000'/
  ! not portable, replaced by:
  bit_pattern_NegInf_i8_hex = ibset(bit_pattern_PosInf_i8_hex,63)

  if (bit_pattern_NegInf_i8_hex /= bit_pattern_NegInf_i8) call abort()
  if (bit_pattern_PosInf_i8_hex /= bit_pattern_PosInf_i8) call abort()

  r = transfer(bit_pattern_PosInf_i8,r)
  if (r /= 1.0_r8_/0.0_r8_) call abort()
  i = transfer(r,i)
  if (bit_pattern_PosInf_i8 /= i) call abort()  

  r = transfer(bit_pattern_NegInf_i8,r)
  if (r /= -1.0_r8_/0.0_r8_) call abort()
  i = transfer(r,i)
  if (bit_pattern_NegInf_i8 /= i) call abort()

  r = transfer(bit_pattern_PosInf_i8_p,r)
  if (r /= 1.0_r8_/0.0_r8_) call abort()
  i = transfer(r,i)
  if (bit_pattern_PosInf_i8_p /= i) call abort()

  r = transfer(bit_pattern_NegInf_i8_p,r)
  if (r /= -1.0_r8_/0.0_r8_) call abort()
  i = transfer(r,i)
  if (bit_pattern_NegInf_i8_p /= i) call abort()
END PROGRAM TestInfinite
