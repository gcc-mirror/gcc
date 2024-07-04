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
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00003fe00ffe3fe0;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000b5207f80;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000b5207f80;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffe218ffffea10;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffff208fffffa02;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xfffff208fffffa02;
  *((unsigned long *)&__m128i_result[1]) = 0xffffe218ffffea10;
  *((unsigned long *)&__m128i_result[0]) = 0xfffff208fffffa02;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000bd3d00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000bd3d00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000bd3d00000000;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0032000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_op2[0]) = 0x2020202020207f7f;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffff0000;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00d3012acc56f9bb;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000000a0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000120002000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_result[1]) = 0x00d3012acc56f9bb;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001021;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_op1[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000014;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op2[1]) = 0x4399d3221a29d3f2;
  *((unsigned long *)&__m128i_op2[0]) = 0xc3818bffe7b7a7b8;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000467fe000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000003ff8;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000003ff8;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000467fef81;
  __m128i_out = __lsx_vmaddwod_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fc0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1e801ffc00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ff020000fff4;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x7fc0000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1e801ffc00000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x3c600000ff800000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfe80000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfe80000000000001;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00007f7f00007f7f;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ffffff03ffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00013fff;
  *((unsigned long *)&__m128i_op1[1]) = 0x000088500000f6a0;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001fffd00000407;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000442900007b4c;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000e22b0000efa4;
  *((unsigned long *)&__m128i_result[1]) = 0x00ffffff03ffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00013fff;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x14ccc6320176a4d2;
  *((unsigned long *)&__m128i_op0[0]) = 0x685670d37e80682a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000007fffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x14ccc6320176a4d2;
  *((unsigned long *)&__m128i_result[0]) = 0x685670d37e80682a;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xc0411fe800000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x601fbfbeffffffff;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffc105d1aa;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffbc19ecca;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff3efa;
  *((unsigned long *)&__m128i_result[0]) = 0xffff0000ffff43e6;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffa7;
  *((unsigned long *)&__m128i_op1[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128i_op1[0]) = 0x00a975be00accf03;
  *((unsigned long *)&__m128i_op2[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128i_op2[0]) = 0x00a975be00accf03;
  *((unsigned long *)&__m128i_result[1]) = 0x0000930400008a10;
  *((unsigned long *)&__m128i_result[0]) = 0x00006f9100007337;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op2[0]) = 0x001000100010c410;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff02ff1bff02ff23;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffffff02fff4;
  *((unsigned long *)&__m128i_op1[1]) = 0xff02ff1bff02ff23;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffffff02fff4;
  *((unsigned long *)&__m128i_op2[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op2[0]) = 0x1e801ffc7fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x7e44bde9b842ff23;
  *((unsigned long *)&__m128i_result[0]) = 0x00011e80007edff8;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffeffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000001;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xe0d56a9774f3ea31;
  *((unsigned long *)&__m128i_op0[0]) = 0xe0dd268932a5edf9;
  *((unsigned long *)&__m128i_op1[1]) = 0xe0d56a9774f3ea31;
  *((unsigned long *)&__m128i_op1[0]) = 0xe0dd268932a5edf9;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xd8248069ffe78077;
  *((unsigned long *)&__m128i_result[1]) = 0xe0d56a9774f3ea31;
  *((unsigned long *)&__m128i_result[0]) = 0xbddaa86803e33c2a;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0028280000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0028280000282800;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x7505853d654185f5;
  *((unsigned long *)&__m128i_op2[0]) = 0x01010000fefe0101;
  *((unsigned long *)&__m128i_result[1]) = 0x0028280000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x012927ffff272800;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ffff7f00ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff007f0101017f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000020000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000183fffffe5;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000073;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000000000002a;
  *((unsigned long *)&__m128i_result[1]) = 0x00ffff7f00ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff007f0101017f;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002bfd9461;
  *((unsigned long *)&__m128i_op2[1]) = 0x3f8000003f800001;
  *((unsigned long *)&__m128i_op2[0]) = 0x3f8000003f800001;
  *((unsigned long *)&__m128i_result[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_result[0]) = 0x3f8000003f800000;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000095896a760000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x006f0efe258ca851;
  *((unsigned long *)&__m128i_op2[1]) = 0xffff7fc8ffff8000;
  *((unsigned long *)&__m128i_op2[0]) = 0xffff200000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000015516a768038;
  *((unsigned long *)&__m128i_result[0]) = 0xffffff9ed2e1c000;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000120002000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_op1[1]) = 0x2000200000013fa0;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000013fa0;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000120002000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100013fa0;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0080008000800080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0080006b00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x001b19b1c9c6da5a;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x001b19b1c9c6da5a;
  *((unsigned long *)&__m128i_result[1]) = 0x0080008000800080;
  *((unsigned long *)&__m128i_result[0]) = 0x008003496dea0c61;
  __m128i_out = __lsx_vmaddwod_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000001ff000001ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000001ff000001ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x000001ff000001ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000001ff000001ff;
  *((unsigned long *)&__m128i_op2[1]) = 0xff80ffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x7ffffffeffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000002fe800000ff;
  *((unsigned long *)&__m128i_result[0]) = 0x7ffffe0100000000;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff00000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfe80000000000001;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op2[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
