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
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffdfff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffdfff;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffe000;
  *((int *)&__m128_op1[0]) = 0x01ffe200;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00010002;
  *((int *)&__m128_op0[2]) = 0x0000fe7d;
  *((int *)&__m128_op0[1]) = 0x00010002;
  *((int *)&__m128_op0[0]) = 0x0000fe02;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x0000007b;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x1223dabf;
  *((int *)&__m128_op0[2]) = 0x4c3b3549;
  *((int *)&__m128_op0[1]) = 0x8e8f8626;
  *((int *)&__m128_op0[0]) = 0xf15be124;
  *((int *)&__m128_op1[3]) = 0xfffffacd;
  *((int *)&__m128_op1[2]) = 0xb6dbecac;
  *((int *)&__m128_op1[1]) = 0x1f5533a6;
  *((int *)&__m128_op1[0]) = 0x94f902c0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xfbffffff;
  *((int *)&__m128_op0[0]) = 0x27001517;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x0000ffff;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xffff53d9;
  *((int *)&__m128_op0[1]) = 0xffff0001;
  *((int *)&__m128_op0[0]) = 0xffff9515;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00010001;
  *((int *)&__m128_op1[2]) = 0x00010001;
  *((int *)&__m128_op1[1]) = 0x00010001;
  *((int *)&__m128_op1[0]) = 0x00010001;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000080;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00df020f;
  *((int *)&__m128_op0[0]) = 0x0078007f;
  *((int *)&__m128_op1[3]) = 0x0037ffd4;
  *((int *)&__m128_op1[2]) = 0x0083ffe5;
  *((int *)&__m128_op1[1]) = 0x001e0052;
  *((int *)&__m128_op1[0]) = 0x001ffff9;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00ff00ff;
  *((int *)&__m128_op0[0]) = 0x00ff00ff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x05452505;
  *((int *)&__m128_op0[1]) = 0x00000004;
  *((int *)&__m128_op0[0]) = 0x442403e4;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000101;
  *((int *)&__m128_op0[2]) = 0x00000101;
  *((int *)&__m128_op0[1]) = 0x00000101;
  *((int *)&__m128_op0[0]) = 0x00000101;
  *((int *)&__m128_op1[3]) = 0x00000002;
  *((int *)&__m128_op1[2]) = 0x00000002;
  *((int *)&__m128_op1[1]) = 0x00000002;
  *((int *)&__m128_op1[0]) = 0x00000002;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00010000;
  *((int *)&__m128_op0[2]) = 0x00010000;
  *((int *)&__m128_op0[1]) = 0x0000cd63;
  *((int *)&__m128_op0[0]) = 0x0000cd63;
  *((int *)&__m128_op1[3]) = 0xffffcd63;
  *((int *)&__m128_op1[2]) = 0xffffcd63;
  *((int *)&__m128_op1[1]) = 0xffffd765;
  *((int *)&__m128_op1[0]) = 0xffffd765;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
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
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffff00ff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffff0000;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0x0000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffff0c8000c212;
  *((unsigned long *)&__m128d_op0[0]) = 0xfefffeff7f002d06;
  *((unsigned long *)&__m128d_op1[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00f0008100800080;
  *((unsigned long *)&__m128d_op0[0]) = 0x00f000807000009e;
  *((unsigned long *)&__m128d_op1[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128d_op1[0]) = 0xfffe00029f9f6061;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffc0800000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000c0010000a186;
  *((unsigned long *)&__m128d_op1[0]) = 0x00067fff0002a207;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sle_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffff80000001;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x3fbf3fbf00007fff;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000003fbf3fbf;
  *((unsigned long *)&__m128d_op1[0]) = 0x7fff7fff7fff7ff8;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x00000000013ec13e;
  *((unsigned long *)&__m128d_op1[0]) = 0xc03fc03fc0ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffff00018d8b;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x67eb85af0000b000;
  *((unsigned long *)&__m128d_op1[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfe3bfb01fe3bfe01;
  *((unsigned long *)&__m128d_op0[0]) = 0xfe03fe3ffe01fa21;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sule_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
