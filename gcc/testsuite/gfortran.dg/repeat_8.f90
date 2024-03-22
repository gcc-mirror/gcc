! { dg-do compile }
! { dg-additional-options "-Wconversion-extra" }
!
! Test fix for PR fortran/96724
!
! Contributed by José Rui Faustino de Sousa 

program repeat_p
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none

  integer, parameter :: n = 20
  integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')

  integer(kind=int8),  parameter :: p08 = int(n, kind=int8)
  integer(kind=int16), parameter :: p16 = int(n, kind=int16)
  integer(kind=int16), parameter :: p32 = int(n, kind=int32)
  integer(kind=int16), parameter :: p64 = int(n, kind=int64)

  integer(kind=int8)  :: i08
  integer(kind=int16) :: i16
  integer(kind=int32) :: i32
  integer(kind=int64) :: i64

  character(len=n,kind=1)    :: c
  character(len=n,kind=ucs4) :: d

  i08 = p08
  c = repeat('X', 20_int8)
  c = repeat('X', i08)
  c = repeat('X', p08)
  c = repeat('X', len08(c))
  d = repeat(ucs4_'X', 20_int8)
  d = repeat(ucs4_'X', i08)
  d = repeat(ucs4_'X', p08)
  d = repeat(ucs4_'X', len08(c))
  i16 = p16
  c = repeat('X', 20_int16)
  c = repeat('X', i16)
  c = repeat('X', p16)
  c = repeat('X', len16(c))
  d = repeat(ucs4_'X', 20_int16)
  d = repeat(ucs4_'X', i16)
  d = repeat(ucs4_'X', p16)
  d = repeat(ucs4_'X', len16(c))
  i32 = p32
  c = repeat('X', 20_int32)
  c = repeat('X', i32)
  c = repeat('X', p32)
  c = repeat('X', len32(c))
  d = repeat(ucs4_'X', 20_int32)
  d = repeat(ucs4_'X', i32)
  d = repeat(ucs4_'X', p32)
  d = repeat(ucs4_'X', len32(c))
  i64 = p64
  c = repeat('X', 20_int64)
  c = repeat('X', i64)
  c = repeat('X', p64)
  c = repeat('X', len64(c))
  d = repeat(ucs4_'X', 20_int64)
  d = repeat(ucs4_'X', i64)
  d = repeat(ucs4_'X', p64)
  d = repeat(ucs4_'X', len64(c))

contains

  function len08(x) result(l)
    character(len=*), intent(in) :: x
    integer(kind=int8) :: l

    l = int(len(x), kind=int8)
  end function len08

  function len16(x) result(l)
    character(len=*), intent(in) :: x
    integer(kind=int16) :: l

    l = int(len(x), kind=int16)
  end function len16

  function len32(x) result(l)
    character(len=*), intent(in) :: x
    integer(kind=int32) :: l

    l = int(len(x), kind=int32)
  end function len32

  function len64(x) result(l)
    character(len=*), intent(in) :: x
    integer(kind=int64) :: l

    l = int(len(x), kind=int64)
  end function len64

  function ulen08(x) result(l)
    character(len=*,kind=ucs4), intent(in) :: x
    integer(kind=int8) :: l

    l = int(len(x), kind=int8)
  end function ulen08

  function ulen16(x) result(l)
    character(len=*,kind=ucs4), intent(in) :: x
    integer(kind=int16) :: l

    l = int(len(x), kind=int16)
  end function ulen16

  function ulen32(x) result(l)
    character(len=*,kind=ucs4), intent(in) :: x
    integer(kind=int32) :: l

    l = int(len(x), kind=int32)
  end function ulen32

  function ulen64(x) result(l)
    character(len=*,kind=ucs4), intent(in) :: x
    integer(kind=int64) :: l

    l = int(len(x), kind=int64)
  end function ulen64

end program repeat_p
