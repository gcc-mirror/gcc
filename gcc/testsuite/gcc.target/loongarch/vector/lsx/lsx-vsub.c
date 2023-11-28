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
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000ff02;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000001fe;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc6ffe000c6fde000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x8080808080808081;
  *((unsigned long *)&__m128i_result[1]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_result[0]) = 0x467f6080467d607f;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000010000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00fe00ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000010000;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff00fe00fe00ff;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff00007fff0000;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000c0dec4d1;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000040223c2e;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfd200ed2fd370775;
  *((unsigned long *)&__m128i_op0[0]) = 0x96198318780e32c5;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffe65ecc1be5bc;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffe65ecc1be5bc;
  *((unsigned long *)&__m128i_result[1]) = 0xfe212874311c22b9;
  *((unsigned long *)&__m128i_result[0]) = 0x971a9dbaacf34d09;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x0);
  *((unsigned long *)&__m128i_op0[1]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_result[0]) = 0x4f4f4f4f4f4f4f4f;
  __m128i_out = __lsx_vsub_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op0[0]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op1[1]) = 0xf000e001bf84df83;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff8e001ff84e703;
  *((unsigned long *)&__m128i_result[1]) = 0x14042382c3ffa481;
  *((unsigned long *)&__m128i_result[0]) = 0x040c238283ff9d01;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0141010101410101;
  *((unsigned long *)&__m128i_op1[0]) = 0x0141010101410101;
  *((unsigned long *)&__m128i_result[1]) = 0xfebffefffebffeff;
  *((unsigned long *)&__m128i_result[0]) = 0xfebffefffebffeff;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_op0[0]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_result[0]) = 0x1111111111111111;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x000700000004fdff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000300000000fdff;
  *((unsigned long *)&__m128i_result[1]) = 0xfff7fffefffa01ff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffbfffefffe01ff;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000000010000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000cd630000cd63;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000329d0000329d;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x08080807f7f7f7f8;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000202020200;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_result[1]) = 0x08080805f5f5f5f8;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000ff00;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00060eb000000006;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000075c00000cf0;
  *((unsigned long *)&__m128i_result[1]) = 0xfffaf1500000fffa;
  *((unsigned long *)&__m128i_result[0]) = 0x0000f8a40000f310;
  __m128i_out = __lsx_vsub_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff100fffc;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000200000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffdf100fffc;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000007f7f7f7f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffff0;
  *((unsigned long *)&__m128i_result[1]) = 0x000000007f7f7f7f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000010;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000800000000000;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00001802041b0013;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x00001802041b0014;
  __m128i_out = __lsx_vsub_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000f7d1000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x773324887fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfff082efffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x88cbdb7780000001;
  __m128i_out = __lsx_vsub_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000001f50000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffe0b0000;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000fffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0010000000000001;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000800080;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000800080;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000015;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0007000000050000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0003000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001fffe0001fefc;
  *((unsigned long *)&__m128i_result[1]) = 0x0006000100040001;
  *((unsigned long *)&__m128i_result[0]) = 0x00010002ffff0105;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000003fffffffd;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000003fffffffd;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000003fffffffe;
  *((unsigned long *)&__m128i_result[0]) = 0x00000003fffffffd;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000feff23560000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000fd1654860000;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363636363abdf16;
  *((unsigned long *)&__m128i_op1[0]) = 0x41f8e08016161198;
  *((unsigned long *)&__m128i_result[1]) = 0x9c9d9b9bbfaa20e9;
  *((unsigned long *)&__m128i_result[0]) = 0xbe081c963e6fee68;
  __m128i_out = __lsx_vsub_q (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
