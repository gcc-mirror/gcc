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
  *((int *)&__m128_op0[2]) = 0x7fff8000;
  *((int *)&__m128_op0[1]) = 0x00010081;
  *((int *)&__m128_op0[0]) = 0x00000005;
  *((unsigned long *)&__m128i_result[1]) = 0x0000020000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010000000100;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xfe02fe02;
  *((int *)&__m128_op0[2]) = 0xfe02fe02;
  *((int *)&__m128_op0[1]) = 0xfe02fe02;
  *((int *)&__m128_op0[0]) = 0xfe02fe02;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000800000008;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x0000000c;
  *((int *)&__m128_op0[2]) = 0x7fff000c;
  *((int *)&__m128_op0[1]) = 0x10001000;
  *((int *)&__m128_op0[0]) = 0x10001000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000008000000080;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000020000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000020000000200;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000020000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000020000000200;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x0c0b0a09;
  *((int *)&__m128_op0[2]) = 0x0b0a0908;
  *((int *)&__m128_op0[1]) = 0x0a090807;
  *((int *)&__m128_op0[0]) = 0x09080706;
  *((unsigned long *)&__m128i_result[1]) = 0x0000008000000080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000008000000080;
  __m128i_out = __lsx_vfclass_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
