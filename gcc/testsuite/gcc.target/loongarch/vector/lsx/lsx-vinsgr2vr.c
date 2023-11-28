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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000007942652524;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x4265252400000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_op1 = 0x0000007942652524;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffff2524ffffffff;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000017fff9000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000210011084;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000017fff9000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000200000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vinsgr2vr_d (__m128i_op0, long_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffff0000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0080000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0080000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0080000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0080000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x5d5d5d5d5d5d5d55;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000001;
  *((unsigned long *)&__m128i_result[0]) = 0x5d5d5d005d5d5d55;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x1);
  *((unsigned long *)&__m128i_op0[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000080000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8000000080000000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x2);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffff0000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_op0[0]) = 0x2020202020202020;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_result[0]) = 0x202020202020ff20;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00fe01fc0005fff4;
  int_op1 = 0x0000000020202020;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000820202020;
  *((unsigned long *)&__m128i_result[0]) = 0x00fe01fc0005fff4;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffbfffffffbf;
  long_op1 = 0x0000000000003a24;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000003a24;
  __m128i_out = __lsx_vinsgr2vr_d (__m128i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ef8000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ef8000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7ef8000000000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_d (__m128i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff000000ff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000001000;
  int_op1 = 0x000000007ff00000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000001000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000020006;
  *((unsigned long *)&__m128i_result[1]) = 0x0000060000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000020006;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000600;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000003;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000003;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000001f1f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff000000001f1f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  long_op1 = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000040;
  __m128i_out = __lsx_vinsgr2vr_d (__m128i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffff0000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000ffffff0000;
  __m128i_out = __lsx_vinsgr2vr_w (__m128i_op0, int_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff000000000000;
  __m128i_out = __lsx_vinsgr2vr_h (__m128i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x04faf60009f5f092;
  *((unsigned long *)&__m128i_op0[0]) = 0x04fafa9200000000;
  int_op1 = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x04faf600fff5f092;
  *((unsigned long *)&__m128i_result[0]) = 0x04fafa9200000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vinsgr2vr_b (__m128i_op0, int_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
