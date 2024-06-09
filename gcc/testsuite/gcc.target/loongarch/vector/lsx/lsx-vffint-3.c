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

  *((unsigned long *)&__m128i_op0[1]) = 0x8493941335f5cc0c;
  *((unsigned long *)&__m128i_op0[0]) = 0x625a7312befcb21e;
  *((unsigned long *)&__m128d_result[1]) = 0x43e092728266beba;
  *((unsigned long *)&__m128d_result[0]) = 0x43d8969cc4afbf2d;
  __m128d_out = __lsx_vffint_d_lu (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_lu (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_lu (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_lu (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001600000016;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001600000016;
  *((int *)&__m128_result[3]) = 0x41b00000;
  *((int *)&__m128_result[2]) = 0x41b00000;
  *((int *)&__m128_result[1]) = 0x41b00000;
  *((int *)&__m128_result[0]) = 0x41b00000;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((int *)&__m128_result[3]) = 0x4f800000;
  *((int *)&__m128_result[2]) = 0x4f800000;
  *((int *)&__m128_result[1]) = 0x4f800000;
  *((int *)&__m128_result[0]) = 0x4f800000;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000442800007b50;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffff0204;
  *((int *)&__m128_result[3]) = 0x46885000;
  *((int *)&__m128_result[2]) = 0x46f6a000;
  *((int *)&__m128_result[1]) = 0x4f800000;
  *((int *)&__m128_result[0]) = 0x4f7fff02;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_wu (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}
