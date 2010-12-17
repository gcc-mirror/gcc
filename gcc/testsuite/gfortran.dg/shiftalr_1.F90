! Test the SHIFTA, SHIFTL and SHIFTR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }

  interface run_shifta
    procedure shifta_1
    procedure shifta_2
    procedure shifta_4
    procedure shifta_8
  end interface
  interface run_shiftl
    procedure shiftl_1
    procedure shiftl_2
    procedure shiftl_4
    procedure shiftl_8
  end interface
  interface run_shiftr
    procedure shiftr_1
    procedure shiftr_2
    procedure shiftr_4
    procedure shiftr_8
  end interface
  interface run_ishft
    procedure ishft_1
    procedure ishft_2
    procedure ishft_4
    procedure ishft_8
  end interface

#define CHECK(I,SHIFT,RESA,RESL,RESR) \
  if (shifta(I,SHIFT) /= RESA) call abort ; \
  if (shiftr(I,SHIFT) /= RESR) call abort ; \
  if (shiftl(I,SHIFT) /= RESL) call abort ; \
  if (run_shifta(I,SHIFT) /= RESA) call abort ; \
  if (run_shiftr(I,SHIFT) /= RESR) call abort ; \
  if (run_shiftl(I,SHIFT) /= RESL) call abort ; \
  if (ishft(I,SHIFT) /= RESL) call abort ; \
  if (ishft(I,-SHIFT) /= RESR) call abort ; \
  if (run_ishft(I,SHIFT) /= RESL) call abort ; \
  if (run_ishft(I,-SHIFT) /= RESR) call abort

  CHECK(0_1,0,0_1,0_1,0_1)
  CHECK(11_1,0,11_1,11_1,11_1)
  CHECK(-11_1,0,-11_1,-11_1,-11_1)
  CHECK(0_1,1,0_1,0_1,0_1)
  CHECK(11_1,1,5_1,22_1,5_1)
  CHECK(11_1,2,2_1,44_1,2_1)
  CHECK(-11_1,1,-6_1,-22_1,huge(0_1)-5_1)

  CHECK(0_2,0,0_2,0_2,0_2)
  CHECK(11_2,0,11_2,11_2,11_2)
  CHECK(-11_2,0,-11_2,-11_2,-11_2)
  CHECK(0_2,1,0_2,0_2,0_2)
  CHECK(11_2,1,5_2,22_2,5_2)
  CHECK(11_2,2,2_2,44_2,2_2)
  CHECK(-11_2,1,-6_2,-22_2,huge(0_2)-5_2)

  CHECK(0_4,0,0_4,0_4,0_4)
  CHECK(11_4,0,11_4,11_4,11_4)
  CHECK(-11_4,0,-11_4,-11_4,-11_4)
  CHECK(0_4,1,0_4,0_4,0_4)
  CHECK(11_4,1,5_4,22_4,5_4)
  CHECK(11_4,2,2_4,44_4,2_4)
  CHECK(-11_4,1,-6_4,-22_4,huge(0_4)-5_4)

  CHECK(0_8,0,0_8,0_8,0_8)
  CHECK(11_8,0,11_8,11_8,11_8)
  CHECK(-11_8,0,-11_8,-11_8,-11_8)
  CHECK(0_8,1,0_8,0_8,0_8)
  CHECK(11_8,1,5_8,22_8,5_8)
  CHECK(11_8,2,2_8,44_8,2_8)
  CHECK(-11_8,1,-6_8,-22_8,huge(0_8)-5_8)

contains

  function shifta_1 (i, shift) result(res)
    integer(kind=1) :: i, res
    integer :: shift
    res = shifta(i,shift)
  end function
  function shiftl_1 (i, shift) result(res)
    integer(kind=1) :: i, res
    integer :: shift
    res = shiftl(i,shift)
  end function
  function shiftr_1 (i, shift) result(res)
    integer(kind=1) :: i, res
    integer :: shift
    res = shiftr(i,shift)
  end function

  function shifta_2 (i, shift) result(res)
    integer(kind=2) :: i, res
    integer :: shift
    res = shifta(i,shift)
  end function
  function shiftl_2 (i, shift) result(res)
    integer(kind=2) :: i, res
    integer :: shift
    res = shiftl(i,shift)
  end function
  function shiftr_2 (i, shift) result(res)
    integer(kind=2) :: i, res
    integer :: shift
    res = shiftr(i,shift)
  end function

  function shifta_4 (i, shift) result(res)
    integer(kind=4) :: i, res
    integer :: shift
    res = shifta(i,shift)
  end function
  function shiftl_4 (i, shift) result(res)
    integer(kind=4) :: i, res
    integer :: shift
    res = shiftl(i,shift)
  end function
  function shiftr_4 (i, shift) result(res)
    integer(kind=4) :: i, res
    integer :: shift
    res = shiftr(i,shift)
  end function

  function shifta_8 (i, shift) result(res)
    integer(kind=8) :: i, res
    integer :: shift
    res = shifta(i,shift)
  end function
  function shiftl_8 (i, shift) result(res)
    integer(kind=8) :: i, res
    integer :: shift
    res = shiftl(i,shift)
  end function
  function shiftr_8 (i, shift) result(res)
    integer(kind=8) :: i, res
    integer :: shift
    res = shiftr(i,shift)
  end function

  function ishft_1 (i, shift) result(res)
    integer(kind=1) :: i, res
    integer :: shift
    res = ishft(i,shift)
  end function
  function ishft_2 (i, shift) result(res)
    integer(kind=2) :: i, res
    integer :: shift
    res = ishft(i,shift)
  end function
  function ishft_4 (i, shift) result(res)
    integer(kind=4) :: i, res
    integer :: shift
    res = ishft(i,shift)
  end function
  function ishft_8 (i, shift) result(res)
    integer(kind=8) :: i, res
    integer :: shift
    res = ishft(i,shift)
  end function

end
