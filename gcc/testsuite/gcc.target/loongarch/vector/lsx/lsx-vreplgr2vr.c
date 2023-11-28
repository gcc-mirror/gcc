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

  int_op0 = 0x0000000059815d00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0400040004000400;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  long_op0 = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000400;
  __m128i_out = __lsx_vreplgr2vr_d (long_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  long_op0 = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_result[0]) = 0x3f8000003f800000;
  __m128i_out = __lsx_vreplgr2vr_d (long_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  long_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_d (long_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000020202020;
  *((unsigned long *)&__m128i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_result[0]) = 0x2020202020202020;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000ff000000ff;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  long_op0 = 0x000000007ff00000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000007ff00000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000007ff00000;
  __m128i_out = __lsx_vreplgr2vr_d (long_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  long_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_d (long_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff00ff00ff00ff;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x000000000000001e;
  *((unsigned long *)&__m128i_result[1]) = 0x1e1e1e1e1e1e1e1e;
  *((unsigned long *)&__m128i_result[0]) = 0x1e1e1e1e1e1e1e1e;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_w (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_h (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_op0 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplgr2vr_b (int_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
