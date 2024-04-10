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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000007f00000004;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000401000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100000004;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x00000000003f0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0404040404040404;
  *((unsigned long *)&__m128i_result[0]) = 0x0404040404000404;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x3f2f1f0f00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff00ff00000000;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000002f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000029;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op2[1]) = 0x000000000000002f;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000029;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffff00;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffff00;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7efefefe82010201;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x418181017dfefdff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffff81;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x52525252adadadad;
  *((unsigned long *)&__m128i_op1[0]) = 0x52525252adadadad;
  *((unsigned long *)&__m128i_op2[1]) = 0x2000000004030201;
  *((unsigned long *)&__m128i_op2[0]) = 0x2000000014131211;
  *((unsigned long *)&__m128i_result[1]) = 0xadadadad52adadad;
  *((unsigned long *)&__m128i_result[0]) = 0xadadadadffffffff;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xbfd10d0d7b6b6b73;
  *((unsigned long *)&__m128i_op1[0]) = 0xc5c534920000c4ed;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xedededededededed;
  *((unsigned long *)&__m128i_result[0]) = 0xedededededededed;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000202020200;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_op1[1]) = 0x04040403fafafafc;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000ff80;
  *((unsigned long *)&__m128i_op2[1]) = 0x00101a1b1c1d1e1f;
  *((unsigned long *)&__m128i_op2[0]) = 0x0807060504030201;
  *((unsigned long *)&__m128i_result[1]) = 0x8000020202000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfc000000000000ff;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000001a0000000b;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x00000080000000ff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff6cffb5ff98ff6e;
  *((unsigned long *)&__m128i_op0[0]) = 0xffd7ff8dffa4ff7a;
  *((unsigned long *)&__m128i_op1[1]) = 0x34947b4b11684f92;
  *((unsigned long *)&__m128i_op1[0]) = 0xee297a731e5c5f86;
  *((unsigned long *)&__m128i_op2[1]) = 0x1f0710301a2b332d;
  *((unsigned long *)&__m128i_op2[0]) = 0x1f20000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffee7a7a9811ff7b;
  *((unsigned long *)&__m128i_result[0]) = 0xff86868686868686;
  __m128i_out = __lsx_vshuf_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_result[1]) = 0x000d000d000d000d;
  *((unsigned long *)&__m128i_result[0]) = 0x000d000d000d000d;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001f002f003f000f;
  *((unsigned long *)&__m128i_op0[0]) = 0x001f002f003f000f;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff7fff7fff7fff;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000100040010001f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0002000300110012;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002bfd9461;
  *((unsigned long *)&__m128i_op2[1]) = 0x00007fff00007fff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00007fff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000300030000001f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0003000300000003;
  *((unsigned long *)&__m128i_op1[1]) = 0x000300037ff000ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0003000300a10003;
  *((unsigned long *)&__m128i_op2[1]) = 0x000000007ff000ff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000003;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0019000000090000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0019000000090000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0909000009090000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0909000009090000;
  *((unsigned long *)&__m128i_op2[1]) = 0x002a05a2f059094a;
  *((unsigned long *)&__m128i_op2[0]) = 0x05ad3ba576eae048;
  *((unsigned long *)&__m128i_result[1]) = 0x909e0480909e048;
  *((unsigned long *)&__m128i_result[0]) = 0x909e0480909e048;
  __m128i_out = __lsx_vshuf_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000030;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000029;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000000000c0;
  *((unsigned long *)&__m128i_op2[0]) = 0x00000001ffffff29;
  *((unsigned long *)&__m128i_result[1]) = 0xffffff29ffffff29;
  *((unsigned long *)&__m128i_result[0]) = 0xffffff2900000001;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000001f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1f54e0ab00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op2[0]) = 0x010101fe0101fe87;
  *((unsigned long *)&__m128i_result[1]) = 0x0101fe870101fe87;
  *((unsigned long *)&__m128i_result[0]) = 0x0101fe8700000000;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002f0000002f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x2000002020000020;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000900000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000002000000003;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_op2[1]) = 0x8000000100000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x8000000000000103;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000000000103;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010380000001;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001000000007;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000002000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001a0000001b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000a0000000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x021b7d24c9678a35;
  *((unsigned long *)&__m128i_op1[0]) = 0x030298a6a1030a49;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000003;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000013;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000011;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002c002400;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffb96bffff57c9;
  *((unsigned long *)&__m128i_op2[0]) = 0xffff6080ffff4417;
  *((unsigned long *)&__m128i_result[1]) = 0xffffb96bffff57c9;
  *((unsigned long *)&__m128i_result[0]) = 0xffffb96bffff57c9;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffff0015172b;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff0015172b;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff0015172b;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_op1[0]) = 0xf0003000f0003000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000007;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000001a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op2[1]) = 0x021b7d24c9678a35;
  *((unsigned long *)&__m128i_op2[0]) = 0x030298a6a1030a49;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff7fff7fff7fff;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000fffe0000fffe;
  *((unsigned long *)&__m128i_op2[1]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_op2[0]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000fffe0000fffe;
  *((unsigned long *)&__m128i_result[0]) = 0x7f8000007f800000;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
