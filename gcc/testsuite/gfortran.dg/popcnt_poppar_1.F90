! { dg-do run }
! { dg-options "-ffree-line-length-none" }

interface runtime_popcnt
  procedure runtime_popcnt_i1
  procedure runtime_popcnt_i2
  procedure runtime_popcnt_i4
  procedure runtime_popcnt_i8
end interface

interface runtime_poppar
  procedure runtime_poppar_i1
  procedure runtime_poppar_i2
  procedure runtime_poppar_i4
  procedure runtime_poppar_i8
end interface

#define CHECK(val,res) \
  if (popcnt(val) /= res) call abort ; \
  if (runtime_popcnt(val) /= res) call abort

#define CHECK2(val) \
  if (poppar(val) /= modulo(popcnt(val),2)) call abort ; \
  if (runtime_poppar(val) /= poppar(val)) call abort

  CHECK(0_1, 0)
  CHECK(0_2, 0)
  CHECK(0_4, 0)
  CHECK(0_8, 0)

  CHECK(1_1, 1)
  CHECK(1_2, 1)
  CHECK(1_4, 1)
  CHECK(1_8, 1)

  CHECK(-1_1,8)
  CHECK(-1_2,16)
  CHECK(-1_4,32)
  CHECK(-1_8,64)

  CHECK(-8_1,8-3)
  CHECK(-8_2,16-3)
  CHECK(-8_4,32-3)
  CHECK(-8_8,64-3)

  CHECK(huge(0_1), 8-1)
  CHECK(huge(0_2), 16-1)
  CHECK(huge(0_4), 32-1)
  CHECK(huge(0_8), 64-1)

  CHECK(-huge(0_1), 2)
  CHECK(-huge(0_2), 2)
  CHECK(-huge(0_4), 2)
  CHECK(-huge(0_8), 2)

  CHECK2(0_1)
  CHECK2(0_2)
  CHECK2(0_4)
  CHECK2(0_8)

  CHECK2(17_1)
  CHECK2(17_2)
  CHECK2(17_4)
  CHECK2(17_8)

  CHECK2(-17_1)
  CHECK2(-17_2)
  CHECK2(-17_4)
  CHECK2(-17_8)

  CHECK2(huge(0_1))
  CHECK2(huge(0_2))
  CHECK2(huge(0_4))
  CHECK2(huge(0_8))

  CHECK2(-huge(0_1))
  CHECK2(-huge(0_2))
  CHECK2(-huge(0_4))
  CHECK2(-huge(0_8))

contains
  integer function runtime_popcnt_i1 (i) result(res)
    integer(kind=1), intent(in) :: i
    res = popcnt(i)
  end function

  integer function runtime_popcnt_i2 (i) result(res)
    integer(kind=2), intent(in) :: i
    res = popcnt(i)
  end function

  integer function runtime_popcnt_i4 (i) result(res)
    integer(kind=4), intent(in) :: i
    res = popcnt(i)
  end function

  integer function runtime_popcnt_i8 (i) result(res)
    integer(kind=8), intent(in) :: i
    res = popcnt(i)
  end function

  integer function runtime_poppar_i1 (i) result(res)
    integer(kind=1), intent(in) :: i
    res = poppar(i)
  end function

  integer function runtime_poppar_i2 (i) result(res)
    integer(kind=2), intent(in) :: i
    res = poppar(i)
  end function

  integer function runtime_poppar_i4 (i) result(res)
    integer(kind=4), intent(in) :: i
    res = poppar(i)
  end function

  integer function runtime_poppar_i8 (i) result(res)
    integer(kind=8), intent(in) :: i
    res = poppar(i)
  end function
end
