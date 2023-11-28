/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lsxintrin.h>

int
main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00003004;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xc3080000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xff800000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffffff;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x01010101;
  *((int *)&__m128_op0[2]) = 0x01010101;
  *((int *)&__m128_op0[1]) = 0x01010101;
  *((int *)&__m128_op0[0]) = 0x01010101;
  *((int *)&__m128_result[3]) = 0xc2fa0000;
  *((int *)&__m128_result[2]) = 0xc2fa0000;
  *((int *)&__m128_result[1]) = 0xc2fa0000;
  *((int *)&__m128_result[0]) = 0xc2fa0000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x01ff01ff;
  *((int *)&__m128_op0[2]) = 0x01ff01ff;
  *((int *)&__m128_op0[1]) = 0x01ff01ff;
  *((int *)&__m128_op0[0]) = 0x01ff01ff;
  *((int *)&__m128_result[3]) = 0xc2f80000;
  *((int *)&__m128_result[2]) = 0xc2f80000;
  *((int *)&__m128_result[1]) = 0xc2f80000;
  *((int *)&__m128_result[0]) = 0xc2f80000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xd46cdc13;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00fe00fe;
  *((int *)&__m128_op0[2]) = 0x000200fe;
  *((int *)&__m128_op0[1]) = 0x00fe00fe;
  *((int *)&__m128_op0[0]) = 0x000200fe;
  *((int *)&__m128_result[3]) = 0xc2fc0000;
  *((int *)&__m128_result[2]) = 0xc3040000;
  *((int *)&__m128_result[1]) = 0xc2fc0000;
  *((int *)&__m128_result[0]) = 0xc3040000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x01010101;
  *((int *)&__m128_op0[0]) = 0x00000100;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xc2fa0000;
  *((int *)&__m128_result[0]) = 0xc30d0000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000014;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000014;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xc3110000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xc3110000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x4e3e1337;
  *((int *)&__m128_op0[0]) = 0x38bb47d2;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0x41e80000;
  *((int *)&__m128_result[0]) = 0xc1600000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xff800000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xff800000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xff800000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00003ff8;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0xff800000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0xc3080000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xf1f181a2;
  *((int *)&__m128_op0[2]) = 0xf1f1f1b0;
  *((int *)&__m128_op0[1]) = 0xf1f1f1f1;
  *((int *)&__m128_op0[0]) = 0xf180f1f1;
  *((int *)&__m128_result[3]) = 0x7fc00000;
  *((int *)&__m128_result[2]) = 0x7fc00000;
  *((int *)&__m128_result[1]) = 0x7fc00000;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vflogb_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}
