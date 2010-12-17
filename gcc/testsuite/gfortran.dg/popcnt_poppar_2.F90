! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

#define CHECK(val,res) \
  if (popcnt(val) /= res) call abort ; \
  if (runtime_popcnt(val) /= res) call abort

#define CHECK2(val) \
  if (poppar(val) /= modulo(popcnt(val),2)) call abort ; \
  if (runtime_poppar(val) /= poppar(val)) call abort

  CHECK(0_16, 0)
  CHECK(1_16, 1)

  CHECK(-1_16,128)
  CHECK(-8_16,128-3)

  CHECK(huge(0_16), 128-1)

  CHECK(-huge(0_16), 2)

  CHECK2(0_16)
  CHECK2(17_16)
  CHECK2(-17_16)
  CHECK2(huge(0_16))
  CHECK2(-huge(0_16))

contains
  integer function runtime_popcnt (i) result(res)
    integer(kind=16), intent(in) :: i
    res = popcnt(i)
  end function

  integer function runtime_poppar (i) result(res)
    integer(kind=16), intent(in) :: i
    res = poppar(i)
  end function
end
