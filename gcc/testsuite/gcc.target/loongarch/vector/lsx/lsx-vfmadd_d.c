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
  *((unsigned long *)&__m128d_op1[1]) = 0x8a228acac14e440a;
  *((unsigned long *)&__m128d_op1[0]) = 0xc77c47cdc0f16549;
  *((unsigned long *)&__m128d_op2[1]) = 0xffffffffd24271c4;
  *((unsigned long *)&__m128d_op2[0]) = 0x2711bad1e8e309ed;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffd24271c4;
  *((unsigned long *)&__m128d_result[0]) = 0x2711bad1e8e309ed;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000040400000383;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffe000ffff1fff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000040400000383;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffe000ffff1fff;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128d_op2[0]) = 0x0001000100000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128d_result[0]) = 0xffffe000ffff1fff;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x00000000003f80b0;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000ff800000;
  *((unsigned long *)&__m128d_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0080200000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000401000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000080000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000080000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000080000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000080000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000000000001e;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m128d_op2[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long *)&__m128d_op2[0]) = 0xfff8000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long *)&__m128d_result[0]) = 0xfff8000000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000009000900;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000009000900;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000009000900;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000009000900;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128d_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x9c83e21a22001818;
  *((unsigned long *)&__m128d_op0[0]) = 0xdd3b8b02563b2d7b;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x7f7f7f007f7f7f00;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x7f7f7f007f7f7f00;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xfff0000000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff00e400ff00e400;
  *((unsigned long *)&__m128d_op0[0]) = 0xff01e41ffff0ffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x5555000054100000;
  *((unsigned long *)&__m128d_op1[0]) = 0x5555000154100155;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xfff0000000000000;
  __m128d_out = __lsx_vfmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000010;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfc01fcfefc02fdf7;
  *((unsigned long *)&__m128d_op0[0]) = 0xfe00fcfffe01fd01;
  *((unsigned long *)&__m128d_op1[1]) = 0xfc01fd1300000001;
  *((unsigned long *)&__m128d_op1[0]) = 0xfe00fd1400010000;
  *((unsigned long *)&__m128d_op2[1]) = 0xfc01fcfefc02fdf7;
  *((unsigned long *)&__m128d_op2[0]) = 0xfe00fcfffe01fd01;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000bd3d00000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0038d800ff000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x00fffe00fffffe00;
  *((unsigned long *)&__m128d_op2[1]) = 0x8000008000008080;
  *((unsigned long *)&__m128d_op2[0]) = 0x8080800000800080;
  *((unsigned long *)&__m128d_result[1]) = 0x0000008000008080;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x00ff80ff00ff80ff;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000900000009;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x000000007ff000ff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000fffffffe;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x67eb85afb2ebb000;
  *((unsigned long *)&__m128d_op1[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000100000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000103;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000100000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000103;
  __m128d_out = __lsx_vfmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}
