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
  *((unsigned long *)&__m128i_result[1]) = 0x0020002000200020;
  *((unsigned long *)&__m128i_result[0]) = 0x0020002000200020;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0040000000ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x0040000000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x36);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x54beed87bc3f2be1;
  *((unsigned long *)&__m128i_op0[0]) = 0x8024d8f6a494afcb;
  *((unsigned long *)&__m128i_result[1]) = 0x54feed87bc3f2be1;
  *((unsigned long *)&__m128i_result[0]) = 0x8064d8f6a494afcb;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x36);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000c400;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x001000100010c410;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x2e2b34ca59fa4c88;
  *((unsigned long *)&__m128i_op0[0]) = 0x3b2c8aefd44be966;
  *((unsigned long *)&__m128i_result[1]) = 0x3e2b34ca59fa4c88;
  *((unsigned long *)&__m128i_result[0]) = 0x3b2c8aefd44be966;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000017fda829;
  *((unsigned long *)&__m128i_result[1]) = 0x0040004000400040;
  *((unsigned long *)&__m128i_result[0]) = 0x0040004017fda869;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x800000ff000000ff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x800000ff080000ff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000000010000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000000010000;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0004000000040000;
  *((unsigned long *)&__m128i_result[0]) = 0x0004000000040000;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf51cf8dad6040188;
  *((unsigned long *)&__m128i_op0[0]) = 0x0982e2daf234ed87;
  *((unsigned long *)&__m128i_result[1]) = 0xf51cf8dad6040188;
  *((unsigned long *)&__m128i_result[0]) = 0x0982eadaf234ed87;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x2b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0002000000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x31);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000006;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8000000080000006;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000080000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000080000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x2b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000010000003f;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000030000003f;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xe5e5e5e5e5e5e5e5;
  *((unsigned long *)&__m128i_op0[0]) = 0xe5e5e5e5e4e4e46d;
  *((unsigned long *)&__m128i_result[1]) = 0xe5e5e5e5e5e5e5e5;
  *((unsigned long *)&__m128i_result[0]) = 0xe5e5e5e5e4e4e46d;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[0]) = 0x1000100010001000;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_result[0]) = 0x0800080008000800;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0100000001000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0100000001000000;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000007fff;
  *((unsigned long *)&__m128i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_result[0]) = 0x2020202020207fff;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000900000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000900013fa0;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x23);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x40f3fa0000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3ff0008000800080;
  *((unsigned long *)&__m128i_result[0]) = 0x40f3fa8000800080;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000040000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x2a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0404040404040404;
  *((unsigned long *)&__m128i_result[0]) = 0xc404040404040404;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000040804000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000040804000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000040a04000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000040a04000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f81e3779b97f4a8;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff02000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1f81e3779b97f4a8;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000008000000080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000008000000080;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0100010001000101;
  *((unsigned long *)&__m128i_result[0]) = 0x0100010001000101;
  __m128i_out = __lsx_vbitseti_h (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000010000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000010000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002711250a27112;
  *((unsigned long *)&__m128i_op0[0]) = 0x00d2701294027112;
  *((unsigned long *)&__m128i_result[1]) = 0x080a791a58aa791a;
  *((unsigned long *)&__m128i_result[0]) = 0x08da781a9c0a791a;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_op0[0]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_result[1]) = 0x1313131313131313;
  *((unsigned long *)&__m128i_result[0]) = 0x1313131313131313;
  __m128i_out = __lsx_vbitseti_b (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x30);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000000;
  __m128i_out = __lsx_vbitseti_d (__m128i_op0, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfff0008000000080;
  *((unsigned long *)&__m128i_result[0]) = 0xfff0008000000080;
  __m128i_out = __lsx_vbitseti_w (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
