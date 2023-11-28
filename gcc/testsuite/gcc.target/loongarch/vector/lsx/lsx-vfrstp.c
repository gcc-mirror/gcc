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

  *((unsigned long *)&__m128i_op0[1]) = 0xfe07e5fefefdddfe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00020100fedd0c00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0005000501800005;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfe07e5fefefdddfe;
  *((unsigned long *)&__m128i_result[0]) = 0x00020100fedd0008;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op1[0]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op2[1]) = 0x03ff03ff03ff03ff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000200010;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000200010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0e7ffffc01fffffc;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000003f803f4;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0e7ffffc01fffffc;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000003f803f4;
  *((unsigned long *)&__m128i_result[1]) = 0x0e7ffffc01fffffc;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001003f803f4;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000800;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000800;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000020000007d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000746400016388;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000586100015567;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0800000200000002;
  *((unsigned long *)&__m128i_result[0]) = 0x000000020000007d;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffff0008;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x61608654a2d4f6da;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ff08ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x36fbdfdcffdcffdc;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000008140c80;
  *((unsigned long *)&__m128i_op2[1]) = 0x1f1f1f1f1f1f1f00;
  *((unsigned long *)&__m128i_op2[0]) = 0x1f1f1f27332b9f00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x36fbdfdcffdc0008;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000aaaa;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000545cab1d;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000081a83bea;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x00d3007c014e00bd;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000aaaa;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000003a0000003a;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x37c0001000000008;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080808080800008;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1f1f1f1f1f1f1f1f;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f1f1f1f1f1f1f1f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x1f1f1f1f1f1f1f1f;
  *((unsigned long *)&__m128i_op2[0]) = 0x1f1f1f1f1f1f1f1f;
  *((unsigned long *)&__m128i_result[1]) = 0x00081f1f1f1f1f1f;
  *((unsigned long *)&__m128i_result[0]) = 0x1f1f1f1f1f1f1f1f;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000400080003fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000bc2000007e10;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000400080003fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000bc2000007e04;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000a752a55;
  *((unsigned long *)&__m128i_op0[0]) = 0x0a753500950fa306;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff14eb54ab;
  *((unsigned long *)&__m128i_op1[0]) = 0x14ea6a002a406a00;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x00007fff7fff8000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000a752a55;
  *((unsigned long *)&__m128i_result[0]) = 0x0a753500950fa306;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_op0[0]) = 0x27b169bbb8145f50;
  *((unsigned long *)&__m128i_op1[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_op1[0]) = 0x27b169bbb8145f50;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_result[0]) = 0x27b169bbb8140001;
  __m128i_out = __lsx_vfrstp_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000155;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff100000000000;
  __m128i_out = __lsx_vfrstp_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
