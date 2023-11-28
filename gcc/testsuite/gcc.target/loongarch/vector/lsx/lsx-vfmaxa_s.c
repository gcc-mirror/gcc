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
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xf436f3f5;
  *((int *)&__m128_op0[0]) = 0x2f4ef4a8;
  *((int *)&__m128_op1[3]) = 0xff800000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0xff800000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xff800000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xff800000;
  *((int *)&__m128_result[0]) = 0x2f4ef4a8;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000800;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000800;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000800;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000800;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xc0c0c000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00800080;
  *((int *)&__m128_op1[2]) = 0x00800080;
  *((int *)&__m128_op1[1]) = 0x0080006b;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00800080;
  *((int *)&__m128_result[2]) = 0xc0c0c000;
  *((int *)&__m128_result[1]) = 0x0080006b;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x80000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x80000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmaxa_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xff01ff01;
  *((int *)&__m128_op1[2]) = 0x0000ff7d;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x0000fffc;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xdfa6e0c6;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xd46cdc13;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x01010101;
  *((int *)&__m128_op0[2]) = 0x01010101;
  *((int *)&__m128_op0[1]) = 0x010101fe;
  *((int *)&__m128_op0[0]) = 0x0101fe87;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0xffff0000;
  *((int *)&__m128_op1[2]) = 0xffff0000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfmina_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}
