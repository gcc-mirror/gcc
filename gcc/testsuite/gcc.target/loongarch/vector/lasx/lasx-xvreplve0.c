/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lasxintrin.h>

int
main ()
{
  __m256i __m256i_op0, __m256i_op1, __m256i_op2, __m256i_out, __m256i_result;
  __m256 __m256_op0, __m256_op1, __m256_op2, __m256_out, __m256_result;
  __m256d __m256d_op0, __m256d_op1, __m256d_op2, __m256d_out, __m256d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffeffffff88;
  *((unsigned long *)&__m256i_op0[2]) = 0x61e0000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffeffffff88;
  *((unsigned long *)&__m256i_op0[0]) = 0x61e0000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffff80fe;
  *((unsigned long *)&__m256i_op0[2]) = 0xd52aaaaa555555ab;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffff80fe;
  *((unsigned long *)&__m256i_op0[0]) = 0xd52aaaaa555555ab;
  *((unsigned long *)&__m256i_result[3]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[2]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[1]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[0]) = 0x555555ab555555ab;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[3]) = 0x8080808080808080;
  *((unsigned long *)&__m256i_result[2]) = 0x8080808080808080;
  *((unsigned long *)&__m256i_result[1]) = 0x8080808080808080;
  *((unsigned long *)&__m256i_result[0]) = 0x8080808080808080;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000003fff3fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x3fff3fff3fff4000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000403f3fff;
  *((unsigned long *)&__m256i_result[3]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[2]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[1]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[0]) = 0x3fff3fff3fff3fff;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_result[3]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_result[1]) = 0x8000000080000001;
  *((unsigned long *)&__m256i_result[0]) = 0x8000000080000001;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020202020;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000003;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0007fd00000f02ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000001fffeff;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00fe00feff02ff;
  *((unsigned long *)&__m256i_result[3]) = 0xff00fe00feff02ff;
  *((unsigned long *)&__m256i_result[2]) = 0xff00fe00feff02ff;
  *((unsigned long *)&__m256i_result[1]) = 0xff00fe00feff02ff;
  *((unsigned long *)&__m256i_result[0]) = 0xff00fe00feff02ff;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfc00ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000100fe000100fe;
  *((unsigned long *)&__m256i_op0[1]) = 0xfc00ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000100fe000100fe;
  *((unsigned long *)&__m256i_result[3]) = 0x00fe00fe00fe00fe;
  *((unsigned long *)&__m256i_result[2]) = 0x00fe00fe00fe00fe;
  *((unsigned long *)&__m256i_result[1]) = 0x00fe00fe00fe00fe;
  *((unsigned long *)&__m256i_result[0]) = 0x00fe00fe00fe00fe;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[0]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000064;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000781;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000064;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000064;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000064;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000064;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000064;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffe20001dfe1f;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffe0047d00e00480;
  *((unsigned long *)&__m256i_op0[2]) = 0x001fc0200060047a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffe0047d00e00480;
  *((unsigned long *)&__m256i_op0[0]) = 0x001fc0200060047a;
  *((unsigned long *)&__m256i_result[3]) = 0x047a047a047a047a;
  *((unsigned long *)&__m256i_result[2]) = 0x047a047a047a047a;
  *((unsigned long *)&__m256i_result[1]) = 0x047a047a047a047a;
  *((unsigned long *)&__m256i_result[0]) = 0x047a047a047a047a;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x037fe01f001fe020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x037fe01f001fe020;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020202020;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_op0[2]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_op0[1]) = 0xff0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_op0[0]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[3]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[2]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[1]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[0]) = 0x0d0d0d0d0d0d0d0d;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[2]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[1]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[0]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_result[3]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_result[2]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_result[1]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_result[0]) = 0x0202010202020102;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[2]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[0]) = 0x00ff00ff00ff00ff;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffe00000001;
  __m256i_out = __lasx_xvreplve0_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[2]) = 0x800080ff800080ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[0]) = 0x800080ff800080ff;
  *((unsigned long *)&__m256i_result[3]) = 0x800080ff800080ff;
  *((unsigned long *)&__m256i_result[2]) = 0x800080ff800080ff;
  *((unsigned long *)&__m256i_result[1]) = 0x800080ff800080ff;
  *((unsigned long *)&__m256i_result[0]) = 0x800080ff800080ff;
  __m256i_out = __lasx_xvreplve0_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve0_q (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffff97a2;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffff97a2;
  *((unsigned long *)&__m256i_result[3]) = 0x97a297a297a297a2;
  *((unsigned long *)&__m256i_result[2]) = 0x97a297a297a297a2;
  *((unsigned long *)&__m256i_result[1]) = 0x97a297a297a297a2;
  *((unsigned long *)&__m256i_result[0]) = 0x97a297a297a297a2;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve0_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
