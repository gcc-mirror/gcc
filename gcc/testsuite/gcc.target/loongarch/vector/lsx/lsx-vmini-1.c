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
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffefffffffc;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffefffefffffffc;
  __m128i_out = __lsx_vmini_b (__m128i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000006f00002f0a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000958aefff895e;
  *((unsigned long *)&__m128i_result[1]) = 0xfafafafafafafafa;
  *((unsigned long *)&__m128i_result[0]) = 0xfafa958aeffa89fa;
  __m128i_out = __lsx_vmini_b (__m128i_op0, -6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmini_b (__m128i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000adadadad;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000adadadad;
  *((unsigned long *)&__m128i_result[1]) = 0xfbfbfbfbadadadad;
  *((unsigned long *)&__m128i_result[0]) = 0xfbfbfbfbadadadad;
  __m128i_out = __lsx_vmini_b (__m128i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_b (__m128i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000202020200;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000202020200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000100;
  __m128i_out = __lsx_vmini_b (__m128i_op0, 5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xf1f1f1f1f1f1f1f1;
  *((unsigned long *)&__m128i_result[0]) = 0xf1f1f1f1f1f1f1f1;
  __m128i_out = __lsx_vmini_b (__m128i_op0, -15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000f50000007500;
  *((unsigned long *)&__m128i_op0[0]) = 0x00007e1600007d98;
  *((unsigned long *)&__m128i_result[1]) = 0x0000f50000000900;
  *((unsigned long *)&__m128i_result[0]) = 0x0000090900000998;
  __m128i_out = __lsx_vmini_b (__m128i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x10f881a20ffd02b0;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ff800000;
  *((unsigned long *)&__m128i_result[1]) = 0xf1f181a2f1f1f1b0;
  *((unsigned long *)&__m128i_result[0]) = 0xf1f1f1f1f180f1f1;
  __m128i_out = __lsx_vmini_b (__m128i_op0, -15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfff6fff6fff6fff6;
  *((unsigned long *)&__m128i_result[0]) = 0xfff6fff6fff6fff6;
  __m128i_out = __lsx_vmini_h (__m128i_op0, -10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1716151416151413;
  *((unsigned long *)&__m128i_op0[0]) = 0x1514131214131211;
  *((unsigned long *)&__m128i_result[1]) = 0xfff3fff3fff3fff3;
  *((unsigned long *)&__m128i_result[0]) = 0xfff3fff3fff3fff3;
  __m128i_out = __lsx_vmini_h (__m128i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_result[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_result[0]) = 0xfefefefefefefefe;
  __m128i_out = __lsx_vmini_h (__m128i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_h (__m128i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_h (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_h (__m128i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff4fffffff4;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff4fffffff4;
  __m128i_out = __lsx_vmini_w (__m128i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff3fffffff3;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff3fffffff3;
  __m128i_out = __lsx_vmini_w (__m128i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001ffff0003ffff0;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fffefffefffef;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffefffef;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff01fe0400000006;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000500000000;
  *((unsigned long *)&__m128i_result[0]) = 0xff01fe0400000005;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffafffffffa;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffafffffffa;
  __m128i_out = __lsx_vmini_w (__m128i_op0, -6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000d0000000d;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080808080808080;
  __m128i_out = __lsx_vmini_w (__m128i_op0, 8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x345002920f3017d6;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff7fffffff7;
  __m128i_out = __lsx_vmini_w (__m128i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x03574e3a62407e03;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff7;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1000000010000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100100000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff1;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff1;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000034;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000006;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000006;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff84fff4ff84fff4;
  *((unsigned long *)&__m128i_op0[0]) = 0x00a6ffceffb60052;
  *((unsigned long *)&__m128i_result[1]) = 0xff84fff4ff84fff4;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff0;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff9;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff9;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x111110ff11111141;
  *((unsigned long *)&__m128i_op0[0]) = 0x1111113111111100;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x55aa55c3d5aa55c4;
  *((unsigned long *)&__m128i_op0[0]) = 0xaa55556fd5aaaac1;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000c;
  *((unsigned long *)&__m128i_result[0]) = 0xaa55556fd5aaaac1;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff4;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffb;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffb;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_result[1]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_result[0]) = 0xfcfcfcdcfcfcfcdc;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001030103;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffc;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000085af0000b000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00017ea200002000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff7;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff4;
  __m128i_out = __lsx_vmini_d (__m128i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff00e400ff00e400;
  *((unsigned long *)&__m128i_op0[0]) = 0xff01e41ffff0ffff;
  *((unsigned long *)&__m128i_result[1]) = 0xff00e400ff00e400;
  *((unsigned long *)&__m128i_result[0]) = 0xff01e41ffff0ffff;
  __m128i_out = __lsx_vmini_d (__m128i_op0, 14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
