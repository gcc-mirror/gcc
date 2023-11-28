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

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffa486c90f;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000058bcc201;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffa486c90f;
  *((unsigned long *)&__m128d_result[0]) = 0x1f52d710bf295626;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffff01ff01;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffff01ff01;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000be00be;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x1f1b917c9f3d5e05;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000001400000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128d_result[0]) = 0x1f81e3779b97f4a8;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128d_op0[0]) = 0xff800000ff800000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128d_op0[0]) = 0x001effae001effae;
  *((unsigned long *)&__m128d_result[1]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128d_result[0]) = 0x2006454690d3de87;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128d_op0[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xff800000ff800000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0001ffff00000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0001ffff0001ffff;
  *((unsigned long *)&__m128d_result[1]) = 0x5ff6a0a40ea8f47c;
  *((unsigned long *)&__m128d_result[0]) = 0x5ff6a0a40e9da42a;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000000000000f;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x61608654a2d4f6da;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00fe000100cf005f;
  *((unsigned long *)&__m128d_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_result[1]) = 0x5f675e96e29a5a60;
  *((unsigned long *)&__m128d_result[0]) = 0x7fff7fff7fff7fff;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrsqrt_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00003f8000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00003f8000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000000fffa0000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000fffa0000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128d_op0[0]) = 0xe593c8c4e593c8c4;
  *((unsigned long *)&__m128d_result[1]) = 0x805ffffe01001fe0;
  *((unsigned long *)&__m128d_result[0]) = 0x9a49e11102834d70;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x5252525252525252;
  *((unsigned long *)&__m128d_op0[0]) = 0x5252dcdcdcdcdcdc;
  *((unsigned long *)&__m128d_result[1]) = 0x2d8bf1f8fc7e3f20;
  *((unsigned long *)&__m128d_result[0]) = 0x2d8b24b936d1b24d;
  __m128d_out = __lsx_vfrecip_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}
