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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000020002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000020002;
  *((unsigned long *)&__m128i_result[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_result[0]) = 0x0303030303030303;
  __m128i_out = __lsx_vmaxi_bu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_result[0]) = 0x1111111111111111;
  __m128i_out = __lsx_vmaxi_bu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_result[0]) = 0x1111111111111111;
  __m128i_out = __lsx_vmaxi_bu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0a0a0a0a0a0a0a;
  __m128i_out = __lsx_vmaxi_bu (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0011001100110011;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x11000f2010000e20;
  *((unsigned long *)&__m128i_op0[0]) = 0x0f000d200e000c20;
  *((unsigned long *)&__m128i_result[1]) = 0x11000f2010000e20;
  *((unsigned long *)&__m128i_result[0]) = 0x0f000d200e000c20;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001ffff0003ffff0;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fffefffefffef;
  *((unsigned long *)&__m128i_result[1]) = 0x001ffff0003ffff0;
  *((unsigned long *)&__m128i_result[0]) = 0x000fffefffefffef;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0005000500050005;
  *((unsigned long *)&__m128i_result[0]) = 0x0005000500050005;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_result[1]) = 0x001d001d20000020;
  *((unsigned long *)&__m128i_result[0]) = 0x001d001d20000020;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00003fff00010000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00123fff00120012;
  *((unsigned long *)&__m128i_result[0]) = 0x0012001200120012;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001a001a001a001a;
  *((unsigned long *)&__m128i_result[0]) = 0x001a001a001a001a;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001e001e001e001e;
  *((unsigned long *)&__m128i_result[0]) = 0x001e001e001e001e;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001d001d001d001d;
  *((unsigned long *)&__m128i_result[0]) = 0x001d001d001d001d;
  __m128i_out = __lsx_vmaxi_hu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000800000008;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001600000016;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001600000016;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f5533a694f902c0;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_result[0]) = 0x1f5533a694f902c0;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x37c0001000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x37c0001000000001;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vmaxi_wu (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xbf8000000000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xcf00000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xbf8000000000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0xcf00000000000000;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000011;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000011;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000001c;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000001c;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000d;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x43d3e0000013e000;
  *((unsigned long *)&__m128i_op0[0]) = 0x43d3e0000013e000;
  *((unsigned long *)&__m128i_result[1]) = 0x43d3e0000013e000;
  *((unsigned long *)&__m128i_result[0]) = 0x43d3e0000013e000;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000100010001007c;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x000100010001007c;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000001d;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000001d;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000001b;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000004;
  __m128i_out = __lsx_vmaxi_du (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
