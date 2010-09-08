! Test the MASKL and MASKR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

#define CHECK(I,KIND,FUNCL,FUNCR,RESL,RESR) \
  if (maskl(I,KIND) /= RESL) call abort ; \
  if (FUNCL(I) /= RESL) call abort ; \
  if (maskr(I,KIND) /= RESR) call abort ; \
  if (FUNCR(I) /= RESR) call abort

  CHECK(0,16,run_maskl16,run_maskr16,0_16,0_16)
  CHECK(1,16,run_maskl16,run_maskr16,-huge(0_16)-1_16,1_16)
  CHECK(2,16,run_maskl16,run_maskr16,(-huge(0_16)-1_16)/2_16,3_16)
  CHECK(3,16,run_maskl16,run_maskr16,(-huge(0_16)-1_16)/4_16,7_16)
  CHECK(int(bit_size(0_16))-2,16,run_maskl16,run_maskr16,-4_16,huge(0_16)/2_16)
  CHECK(int(bit_size(0_16))-1,16,run_maskl16,run_maskr16,-2_16,huge(0_16))
  CHECK(int(bit_size(0_16)),16,run_maskl16,run_maskr16,-1_16,-1_16)

contains

  pure integer(kind=16) function run_maskl16(i) result(res)
    integer, intent(in) :: i
    res = maskl(i,kind=16)
  end function
  pure integer(kind=16) function run_maskr16(i) result(res)
    integer, intent(in) :: i
    res = maskr(i,kind=16)
  end function

end
