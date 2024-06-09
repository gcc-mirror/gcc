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

  *((unsigned long *)&__m128d_op0[1]) = 0x0001ffff0001ffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0001ffff0001ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000100;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000002;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128d_op0[0]) = 0xff00000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000008;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x14ccc6320176a4d2;
  *((unsigned long *)&__m128d_op0[0]) = 0x685670d37e80682a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000080;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vfclass_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
