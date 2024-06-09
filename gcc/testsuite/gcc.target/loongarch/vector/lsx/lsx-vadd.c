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
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000201000000000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000fc0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x0002010000fc000b;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000017fda829;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000017fda829;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000001fffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x7f7f7f7f00107f04;
  *((unsigned long *)&__m128i_op1[0]) = 0x7f0000fd7f0000fd;
  *((unsigned long *)&__m128i_result[1]) = 0x7e7e7e7eff0f7f04;
  *((unsigned long *)&__m128i_result[0]) = 0x7f0000fd7f01fffb;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0080000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xf4b6f3f52f4ef4a8;
  *((unsigned long *)&__m128i_op1[1]) = 0x195f307a5d04acbb;
  *((unsigned long *)&__m128i_op1[0]) = 0x6a1a3fbb3c90260e;
  *((unsigned long *)&__m128i_result[1]) = 0x19df307a5d04acbb;
  *((unsigned long *)&__m128i_result[0]) = 0x5ed032b06bde1ab6;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5555001400005111;
  *((unsigned long *)&__m128i_op0[0]) = 0xffabbeab55110140;
  *((unsigned long *)&__m128i_op1[1]) = 0x5555001400005111;
  *((unsigned long *)&__m128i_op1[0]) = 0xffabbeab55110140;
  *((unsigned long *)&__m128i_result[1]) = 0xaaaa00280000a222;
  *((unsigned long *)&__m128i_result[0]) = 0xfe567c56aa220280;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xf51cf8dad6040188;
  *((unsigned long *)&__m128i_op1[0]) = 0x0982e2daf234ed87;
  *((unsigned long *)&__m128i_result[1]) = 0xf51cf8dad6040188;
  *((unsigned long *)&__m128i_result[0]) = 0x0982e2daf234ed87;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000490000004d;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000073;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000002a;
  *((unsigned long *)&__m128i_result[1]) = 0x00000049000000c0;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001ffffff29;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000bd3d;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007fff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000bd30;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000d7fff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000007a6d;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000dfefe0000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffd000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffd000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfefa000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_result[0]) = 0xfefefefefefefefe;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0038000000051fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x003c000000022021;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff0101ffffe000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffffa0204000;
  *((unsigned long *)&__m128i_result[1]) = 0x7f370101ff04ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7f3bffffa0226021;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1baf8eabd26bc629;
  *((unsigned long *)&__m128i_op0[0]) = 0x1c2640b9a8e9fb49;
  *((unsigned long *)&__m128i_op1[1]) = 0x0002dab8746acf8e;
  *((unsigned long *)&__m128i_op1[0]) = 0x00036dd1c5c15856;
  *((unsigned long *)&__m128i_result[1]) = 0x1bb1686346d595b7;
  *((unsigned long *)&__m128i_result[0]) = 0x1c29ad8a6daa539f;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000003;
  *((unsigned long *)&__m128i_op1[1]) = 0xfeffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xfeffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfeffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfeffffffffff0002;
  __m128i_out = __lsx_vadd_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001ffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff800000c3080000;
  *((unsigned long *)&__m128i_result[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_result[0]) = 0xff81ffffc3080000;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x004200a000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x004200a000200001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x004200a000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x004200a000200001;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001f0000001f;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff00ff00ff00ff;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0029aeaca57d74e6;
  *((unsigned long *)&__m128i_op0[0]) = 0xdbe332365392c686;
  *((unsigned long *)&__m128i_op1[1]) = 0x000056f64adb9464;
  *((unsigned long *)&__m128i_op1[0]) = 0x29ca096f235819c2;
  *((unsigned long *)&__m128i_result[1]) = 0x002a05a2f059094a;
  *((unsigned long *)&__m128i_result[0]) = 0x05ad3ba576eae048;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000040d;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001300000013;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000100;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000100;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000100;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001000000ff;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000300000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_result[1]) = 0x00000002fffffffb;
  *((unsigned long *)&__m128i_result[0]) = 0x000000010000fffb;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000060000000e;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001201fe01e9;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000060000000e;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001201fe01e9;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000c0000001c;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002403fc03d2;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfff1000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0xfff1000100010001;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_op1[0]) = 0xa352bfac9269e0aa;
  *((unsigned long *)&__m128i_result[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_result[0]) = 0xa352bfac9269e0aa;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffa;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001001100110068;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0001001100110067;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x379674c000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3789f68000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x379674c000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x3789f68000000000;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000555889;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000002580f01;
  *((unsigned long *)&__m128i_op1[1]) = 0x00060fbf02040fbf;
  *((unsigned long *)&__m128i_op1[0]) = 0x00020fbf02000fbf;
  *((unsigned long *)&__m128i_result[1]) = 0x00060fbf02596848;
  *((unsigned long *)&__m128i_result[0]) = 0x00020fbf04581ec0;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001388928513889;
  *((unsigned long *)&__m128i_op0[0]) = 0x006938094a013889;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001388928513889;
  *((unsigned long *)&__m128i_op1[0]) = 0x006938094a013889;
  *((unsigned long *)&__m128i_result[1]) = 0x0002711250a27112;
  *((unsigned long *)&__m128i_result[0]) = 0x00d2701294027112;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op0[0]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op1[1]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_op1[0]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_result[1]) = 0x202544f490f2de35;
  *((unsigned long *)&__m128i_result[0]) = 0x202544f490f2de35;
  __m128i_out = __lsx_vadd_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
