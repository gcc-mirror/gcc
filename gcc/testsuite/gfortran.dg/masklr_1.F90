! Test the MASKL and MASKR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }

#define CHECK(I,KIND,FUNCL,FUNCR,RESL,RESR) \
  if (maskl(I,KIND) /= RESL) call abort ; \
  if (FUNCL(I) /= RESL) call abort ; \
  if (maskr(I,KIND) /= RESR) call abort ; \
  if (FUNCR(I) /= RESR) call abort

  CHECK(0,1,run_maskl1,run_maskr1,0_1,0_1)
  CHECK(1,1,run_maskl1,run_maskr1,-huge(0_1)-1_1,1_1)
  CHECK(2,1,run_maskl1,run_maskr1,(-huge(0_1)-1_1)/2_1,3_1)
  CHECK(3,1,run_maskl1,run_maskr1,(-huge(0_1)-1_1)/4_1,7_1)
  CHECK(int(bit_size(0_1))-2,1,run_maskl1,run_maskr1,-4_1,huge(0_1)/2_1)
  CHECK(int(bit_size(0_1))-1,1,run_maskl1,run_maskr1,-2_1,huge(0_1))
  CHECK(int(bit_size(0_1)),1,run_maskl1,run_maskr1,-1_1,-1_1)

  CHECK(0,2,run_maskl2,run_maskr2,0_2,0_2)
  CHECK(1,2,run_maskl2,run_maskr2,-huge(0_2)-1_2,1_2)
  CHECK(2,2,run_maskl2,run_maskr2,(-huge(0_2)-1_2)/2_2,3_2)
  CHECK(3,2,run_maskl2,run_maskr2,(-huge(0_2)-1_2)/4_2,7_2)
  CHECK(int(bit_size(0_2))-2,2,run_maskl2,run_maskr2,-4_2,huge(0_2)/2_2)
  CHECK(int(bit_size(0_2))-1,2,run_maskl2,run_maskr2,-2_2,huge(0_2))
  CHECK(int(bit_size(0_2)),2,run_maskl2,run_maskr2,-1_2,-1_2)

  CHECK(0,4,run_maskl4,run_maskr4,0_4,0_4)
  CHECK(1,4,run_maskl4,run_maskr4,-huge(0_4)-1_4,1_4)
  CHECK(2,4,run_maskl4,run_maskr4,(-huge(0_4)-1_4)/2_4,3_4)
  CHECK(3,4,run_maskl4,run_maskr4,(-huge(0_4)-1_4)/4_4,7_4)
  CHECK(int(bit_size(0_4))-2,4,run_maskl4,run_maskr4,-4_4,huge(0_4)/2_4)
  CHECK(int(bit_size(0_4))-1,4,run_maskl4,run_maskr4,-2_4,huge(0_4))
  CHECK(int(bit_size(0_4)),4,run_maskl4,run_maskr4,-1_4,-1_4)

  CHECK(0,8,run_maskl8,run_maskr8,0_8,0_8)
  CHECK(1,8,run_maskl8,run_maskr8,-huge(0_8)-1_8,1_8)
  CHECK(2,8,run_maskl8,run_maskr8,(-huge(0_8)-1_8)/2_8,3_8)
  CHECK(3,8,run_maskl8,run_maskr8,(-huge(0_8)-1_8)/4_8,7_8)
  CHECK(int(bit_size(0_8))-2,8,run_maskl8,run_maskr8,-4_8,huge(0_8)/2_8)
  CHECK(int(bit_size(0_8))-1,8,run_maskl8,run_maskr8,-2_8,huge(0_8))
  CHECK(int(bit_size(0_8)),8,run_maskl8,run_maskr8,-1_8,-1_8)

contains

  pure integer(kind=1) function run_maskl1(i) result(res)
    integer, intent(in) :: i
    res = maskl(i,kind=1)
  end function
  pure integer(kind=1) function run_maskr1(i) result(res)
    integer, intent(in) :: i
    res = maskr(i,kind=1)
  end function

  pure integer(kind=2) function run_maskl2(i) result(res)
    integer, intent(in) :: i
    res = maskl(i,kind=2)
  end function
  pure integer(kind=2) function run_maskr2(i) result(res)
    integer, intent(in) :: i
    res = maskr(i,kind=2)
  end function

  pure integer(kind=4) function run_maskl4(i) result(res)
    integer, intent(in) :: i
    res = maskl(i,kind=4)
  end function
  pure integer(kind=4) function run_maskr4(i) result(res)
    integer, intent(in) :: i
    res = maskr(i,kind=4)
  end function

  pure integer(kind=8) function run_maskl8(i) result(res)
    integer, intent(in) :: i
    res = maskl(i,kind=8)
  end function
  pure integer(kind=8) function run_maskr8(i) result(res)
    integer, intent(in) :: i
    res = maskr(i,kind=8)
  end function

end
