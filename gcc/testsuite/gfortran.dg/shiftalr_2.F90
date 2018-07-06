! Test the SHIFTA, SHIFTL and SHIFTR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

  implicit none

#define CHECK(I,SHIFT,RESA,RESL,RESR) \
  if (shifta(I,SHIFT) /= RESA) STOP 1; \
  if (shiftr(I,SHIFT) /= RESR) STOP 2; \
  if (shiftl(I,SHIFT) /= RESL) STOP 3; \
  if (run_shifta(I,SHIFT) /= RESA) STOP 4; \
  if (run_shiftr(I,SHIFT) /= RESR) STOP 5; \
  if (run_shiftl(I,SHIFT) /= RESL) STOP 6; \
  if (ishft(I,SHIFT) /= RESL) STOP 7; \
  if (ishft(I,-SHIFT) /= RESR) STOP 8; \
  if (run_ishft(I,SHIFT) /= RESL) STOP 9; \
  if (run_ishft(I,-SHIFT) /= RESR) STOP 10

  CHECK(0_16,0,0_16,0_16,0_16)
  CHECK(11_16,0,11_16,11_16,11_16)
  CHECK(-11_16,0,-11_16,-11_16,-11_16)
  CHECK(0_16,1,0_16,0_16,0_16)
  CHECK(11_16,1,5_16,22_16,5_16)
  CHECK(11_16,2,2_16,44_16,2_16)
  CHECK(-11_16,1,-6_16,-22_16,huge(0_16)-5_16)

contains

  function run_shifta (i, shift) result(res)
    integer(kind=16) :: i, res
    integer :: shift
    res = shifta(i,shift)
  end function
  function run_shiftl (i, shift) result(res)
    integer(kind=16) :: i, res
    integer :: shift
    res = shiftl(i,shift)
  end function
  function run_shiftr (i, shift) result(res)
    integer(kind=16) :: i, res
    integer :: shift
    res = shiftr(i,shift)
  end function
  function run_ishft (i, shift) result(res)
    integer(kind=16) :: i, res
    integer :: shift
    res = ishft(i,shift)
  end function

end
