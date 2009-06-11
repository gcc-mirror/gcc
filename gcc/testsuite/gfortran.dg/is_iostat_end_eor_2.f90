! Check that we correctly simplify IS_IOSTAT_END and IS_IOSTAT_EOR.
! Not very useful, but required by the standards
!
! This test relies on the error numbers for END and EOR being -1 and -2.
! This is good to actual
!
! { dg-do compile }
!

  use iso_fortran_env, only : iostat_end, iostat_eor
  implicit none

  integer(kind=merge(4, 0, is_iostat_end(-1))) :: a
  integer(kind=merge(4, 0, is_iostat_end(-1_1))) :: b
  integer(kind=merge(4, 0, is_iostat_end(-1_2))) :: c
  integer(kind=merge(4, 0, is_iostat_end(-1_4))) :: d
  integer(kind=merge(4, 0, is_iostat_end(-1_8))) :: e

  integer(kind=merge(4, 0, is_iostat_eor(-2))) :: f
  integer(kind=merge(4, 0, is_iostat_eor(-2_1))) :: g
  integer(kind=merge(4, 0, is_iostat_eor(-2_2))) :: h
  integer(kind=merge(4, 0, is_iostat_eor(-2_4))) :: i
  integer(kind=merge(4, 0, is_iostat_eor(-2_8))) :: j

  integer(kind=merge(0, 4, is_iostat_eor(-1))) :: k
  integer(kind=merge(0, 4, is_iostat_end(-2))) :: l

  integer(kind=merge(0, 4, is_iostat_eor(0))) :: m
  integer(kind=merge(0, 4, is_iostat_end(0))) :: n

  integer(kind=merge(4, 0, is_iostat_end(0))) :: o ! { dg-error "not supported for type" }
  integer(kind=merge(4, 0, is_iostat_eor(0))) :: p ! { dg-error "not supported for type" }

  integer(kind=merge(4, 0, is_iostat_eor(iostat_eor))) :: q
  integer(kind=merge(4, 0, is_iostat_end(iostat_end))) :: r
  integer(kind=merge(0, 4, is_iostat_end(iostat_eor))) :: s
  integer(kind=merge(0, 4, is_iostat_eor(iostat_end))) :: t

  end
