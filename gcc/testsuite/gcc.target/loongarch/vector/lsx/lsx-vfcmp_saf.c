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

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0x80000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00a300a3;
  *((int *)&__m128_op1[2]) = 0x00a300a3;
  *((int *)&__m128_op1[1]) = 0x00a300a3;
  *((int *)&__m128_op1[0]) = 0x00a300a3;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xb8ec43be;
  *((int *)&__m128_op1[2]) = 0xfe38e64b;
  *((int *)&__m128_op1[1]) = 0x6477d042;
  *((int *)&__m128_op1[0]) = 0x343cce24;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000010;
  *((int *)&__m128_op0[2]) = 0x00100010;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00020000;
  *((int *)&__m128_op0[0]) = 0xffff0001;
  *((int *)&__m128_op1[3]) = 0x63636363;
  *((int *)&__m128_op1[2]) = 0x63636363;
  *((int *)&__m128_op1[1]) = 0x63636363;
  *((int *)&__m128_op1[0]) = 0x63636363;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x03080401;
  *((int *)&__m128_op0[2]) = 0x0d090107;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0a0a0a000a0a0a00;
  *((unsigned long *)&__m128d_op1[0]) = 0x0a0a0a0009090900;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000ffffff01;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x80808080806b000b;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000007ff000ff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x67157b5100005000;
  *((unsigned long *)&__m128d_op1[0]) = 0x387c7e0a133f2000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xf359f359f359f359;
  *((unsigned long *)&__m128d_op0[0]) = 0xf359f359f359f359;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0177fff0fffffff0;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000011ff8bc;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_saf_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
