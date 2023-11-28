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

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128d_op1[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128d_op1[0]) = 0x0400040004000400;
  *((unsigned long *)&__m128d_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128d_result[0]) = 0x0400040004000400;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x01ff01ff01ff01ff;
  *((unsigned long *)&__m128d_op0[0]) = 0x01ff01ff01ff01ff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x01ff01ff01ff01ff;
  *((unsigned long *)&__m128d_result[0]) = 0x01ff01ff01ff01ff;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128d_result[0]) = 0xfffcfffcfffcfffc;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x000000000000ffff;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128d_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op1[0]) = 0xfdfef9ff0efff900;
  *((unsigned long *)&__m128d_result[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128d_result[0]) = 0x6363636363636363;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128d_op0[0]) = 0xa352bfac9269e0aa;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x98147a504d145000;
  *((unsigned long *)&__m128d_op0[0]) = 0x377b810912c0e000;
  *((unsigned long *)&__m128d_op1[1]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128d_op1[0]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128d_result[1]) = 0x98147a504d145000;
  *((unsigned long *)&__m128d_result[0]) = 0x377b810912c0e000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x4399d3221a29d3f2;
  *((unsigned long *)&__m128d_op0[0]) = 0xc3818bffe7b7a7b8;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x4399d3221a29d3f2;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x17c64aaef639f093;
  *((unsigned long *)&__m128d_op0[0]) = 0xdb8f439722ec502d;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x17c64aaef639f093;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x10f881a20ffd02b0;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000ff800000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x10f881a20ffd02b0;
  *((unsigned long *)&__m128d_result[0]) = 0x00000000ff800000;
  __m128d_out = __lsx_vfmax_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000c000ffffc000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000958affff995d;
  *((unsigned long *)&__m128d_result[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128d_result[0]) = 0x0000958affff995d;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x1748c4f9ed1a5870;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmin_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}
