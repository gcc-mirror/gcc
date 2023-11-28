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

  *((int *)&__m128_op0[3]) = 0x004200a0;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x004200a0;
  *((int *)&__m128_op0[0]) = 0x00200001;
  *((int *)&__m128_op1[3]) = 0x004200a0;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x004200a0;
  *((int *)&__m128_op1[0]) = 0x00200000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00010001;
  *((int *)&__m128_op1[2]) = 0x0001007c;
  *((int *)&__m128_op1[1]) = 0x00010001;
  *((int *)&__m128_op1[0]) = 0x00010001;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
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
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x80808080;
  *((int *)&__m128_op1[2]) = 0x80808080;
  *((int *)&__m128_op1[1]) = 0x80808080;
  *((int *)&__m128_op1[0]) = 0x80808080;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8000800080008000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
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
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xfffffffc;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xfffffffc;
  *((int *)&__m128_op1[3]) = 0x00000001;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000103;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcvt_h_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000049000000c0;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000ffffff29;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000100000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x7ff0000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000002c002400;
  *((unsigned long *)&__m128d_op1[1]) = 0x7ef400ad21fc7081;
  *((unsigned long *)&__m128d_op1[0]) = 0x28bf0351ec69b5f2;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000dc300003ffb;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000dc300003ffb;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000ffff3fbfffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x7fffffff7fffffff;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x7ffffffb;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xbba0c07b51230d5c;
  *((unsigned long *)&__m128d_op0[0]) = 0xa15f3f9e8763c2b9;
  *((unsigned long *)&__m128d_op1[1]) = 0xbba0c07b51230d5c;
  *((unsigned long *)&__m128d_op1[0]) = 0xa15f3f9e8763c2b9;
  *((int *)&__m128_result[3]) = 0x9d0603db;
  *((int *)&__m128_result[2]) = 0x80000000;
  *((int *)&__m128_result[1]) = 0x9d0603db;
  *((int *)&__m128_result[0]) = 0x80000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128d_op1[1]) = 0x8101010181010101;
  *((unsigned long *)&__m128d_op1[0]) = 0x8101010181010101;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x80000000;
  *((int *)&__m128_result[0]) = 0x80000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffc00000ff800000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffffff;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffdfffe80008000;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000ff00000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0xffeffff4;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000090;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000090;
  *((unsigned long *)&__m128d_op1[1]) = 0x004eff6200d2ff76;
  *((unsigned long *)&__m128d_op1[0]) = 0xff70002800be00a0;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0xff800000;
  __m128_out = __lsx_vfcvt_s_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}
