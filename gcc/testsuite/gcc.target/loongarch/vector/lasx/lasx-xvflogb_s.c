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

  *((int *)&__m256_op0[7]) = 0xffffffff;
  *((int *)&__m256_op0[6]) = 0xffffffff;
  *((int *)&__m256_op0[5]) = 0xffffffff;
  *((int *)&__m256_op0[4]) = 0xffffffff;
  *((int *)&__m256_op0[3]) = 0xffffffff;
  *((int *)&__m256_op0[2]) = 0xffffffff;
  *((int *)&__m256_op0[1]) = 0xffffffff;
  *((int *)&__m256_op0[0]) = 0xffffffff;
  *((int *)&__m256_result[7]) = 0xffffffff;
  *((int *)&__m256_result[6]) = 0xffffffff;
  *((int *)&__m256_result[5]) = 0xffffffff;
  *((int *)&__m256_result[4]) = 0xffffffff;
  *((int *)&__m256_result[3]) = 0xffffffff;
  *((int *)&__m256_result[2]) = 0xffffffff;
  *((int *)&__m256_result[1]) = 0xffffffff;
  *((int *)&__m256_result[0]) = 0xffffffff;
  __m256_out = __lasx_xvflogb_s (__m256_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((int *)&__m256_op0[7]) = 0x10101010;
  *((int *)&__m256_op0[6]) = 0x10101012;
  *((int *)&__m256_op0[5]) = 0x10101010;
  *((int *)&__m256_op0[4]) = 0x10101012;
  *((int *)&__m256_op0[3]) = 0x10101010;
  *((int *)&__m256_op0[2]) = 0x10101093;
  *((int *)&__m256_op0[1]) = 0x11111111;
  *((int *)&__m256_op0[0]) = 0x11111113;
  *((int *)&__m256_result[7]) = 0xc2be0000;
  *((int *)&__m256_result[6]) = 0xc2be0000;
  *((int *)&__m256_result[5]) = 0xc2be0000;
  *((int *)&__m256_result[4]) = 0xc2be0000;
  *((int *)&__m256_result[3]) = 0xc2be0000;
  *((int *)&__m256_result[2]) = 0xc2be0000;
  *((int *)&__m256_result[1]) = 0xc2ba0000;
  *((int *)&__m256_result[0]) = 0xc2ba0000;
  __m256_out = __lasx_xvflogb_s (__m256_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((int *)&__m256_op0[7]) = 0x00000000;
  *((int *)&__m256_op0[6]) = 0x00000000;
  *((int *)&__m256_op0[5]) = 0x00000000;
  *((int *)&__m256_op0[4]) = 0x00000000;
  *((int *)&__m256_op0[3]) = 0x00000000;
  *((int *)&__m256_op0[2]) = 0x00000000;
  *((int *)&__m256_op0[1]) = 0x00000000;
  *((int *)&__m256_op0[0]) = 0x00000000;
  *((int *)&__m256_result[7]) = 0xff800000;
  *((int *)&__m256_result[6]) = 0xff800000;
  *((int *)&__m256_result[5]) = 0xff800000;
  *((int *)&__m256_result[4]) = 0xff800000;
  *((int *)&__m256_result[3]) = 0xff800000;
  *((int *)&__m256_result[2]) = 0xff800000;
  *((int *)&__m256_result[1]) = 0xff800000;
  *((int *)&__m256_result[0]) = 0xff800000;
  __m256_out = __lasx_xvflogb_s (__m256_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((int *)&__m256_op0[7]) = 0x00000000;
  *((int *)&__m256_op0[6]) = 0x00000000;
  *((int *)&__m256_op0[5]) = 0x00000000;
  *((int *)&__m256_op0[4]) = 0x00000000;
  *((int *)&__m256_op0[3]) = 0x00000000;
  *((int *)&__m256_op0[2]) = 0x00000000;
  *((int *)&__m256_op0[1]) = 0x00000000;
  *((int *)&__m256_op0[0]) = 0x00000000;
  *((int *)&__m256_result[7]) = 0xff800000;
  *((int *)&__m256_result[6]) = 0xff800000;
  *((int *)&__m256_result[5]) = 0xff800000;
  *((int *)&__m256_result[4]) = 0xff800000;
  *((int *)&__m256_result[3]) = 0xff800000;
  *((int *)&__m256_result[2]) = 0xff800000;
  *((int *)&__m256_result[1]) = 0xff800000;
  *((int *)&__m256_result[0]) = 0xff800000;
  __m256_out = __lasx_xvflogb_s (__m256_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((int *)&__m256_op0[7]) = 0x00000000;
  *((int *)&__m256_op0[6]) = 0x00000000;
  *((int *)&__m256_op0[5]) = 0x00000087;
  *((int *)&__m256_op0[4]) = 0x00000000;
  *((int *)&__m256_op0[3]) = 0x00000000;
  *((int *)&__m256_op0[2]) = 0x00000000;
  *((int *)&__m256_op0[1]) = 0x00000087;
  *((int *)&__m256_op0[0]) = 0x00000000;
  *((int *)&__m256_result[7]) = 0xff800000;
  *((int *)&__m256_result[6]) = 0xff800000;
  *((int *)&__m256_result[5]) = 0xc30e0000;
  *((int *)&__m256_result[4]) = 0xff800000;
  *((int *)&__m256_result[3]) = 0xff800000;
  *((int *)&__m256_result[2]) = 0xff800000;
  *((int *)&__m256_result[1]) = 0xc30e0000;
  *((int *)&__m256_result[0]) = 0xff800000;
  __m256_out = __lasx_xvflogb_s (__m256_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  return 0;
}
