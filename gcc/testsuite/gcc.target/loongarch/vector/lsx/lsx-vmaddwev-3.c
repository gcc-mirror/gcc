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
  *((unsigned long *)&__m128i_op2[1]) = 0xc0c00000c0c00000;
  *((unsigned long *)&__m128i_op2[0]) = 0xc0c00c01c2cd0009;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7f78787f00f7f700;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000f7f700f7f700;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000400;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000000000040d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000080003f80ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op2[1]) = 0x3ff0010000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x3ff0010000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000080003f80ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xff81ff82ff810081;
  *((unsigned long *)&__m128i_op2[0]) = 0xff82ff810081ff81;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op0[0]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffff8000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffff8000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffff8000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x841f000fc28f801f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x841f000fc28f801f;
  *((unsigned long *)&__m128i_op2[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op2[0]) = 0xe593c8c4e593c8c4;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x76ecfc8b85ac78db;
  __m128i_out = __lsx_vmaddwev_h_bu_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000fff3;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000000000040d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000010400;
  __m128i_out = __lsx_vmaddwev_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_result[1]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100013fa0;
  __m128i_out = __lsx_vmaddwev_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000002b0995850;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff80005613;
  *((unsigned long *)&__m128i_op1[0]) = 0x007f800000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffff80005613;
  *((unsigned long *)&__m128i_op2[0]) = 0x007f800000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff00011cf0c569;
  *((unsigned long *)&__m128i_result[0]) = 0xc0000002b0995850;
  __m128i_out = __lsx_vmaddwev_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000ffffff81fe;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffff00ffff7e01;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000fffe01fd02;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00fe00fffe86f901;
  __m128i_out = __lsx_vmaddwev_w_hu_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000004000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff8004000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xbfffbfffbfffbffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xbfffbfffbfffbffe;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xbfffbfffbfffbffe;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffd3000000130000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffd3000000130000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffd3000000130000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffd3000000130000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffd3000000130000;
  *((unsigned long *)&__m128i_result[0]) = 0xffd3000000130000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_op2[0]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_result[1]) = 0x00000f02e1f80f04;
  *((unsigned long *)&__m128i_result[0]) = 0x00000f02e1f80f04;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffefffe00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffe00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x98147a504d145000;
  *((unsigned long *)&__m128i_op1[0]) = 0x377b810912c0e000;
  *((unsigned long *)&__m128i_op2[1]) = 0x5a57bacbd7e39680;
  *((unsigned long *)&__m128i_op2[0]) = 0x6bae051ffed76001;
  *((unsigned long *)&__m128i_result[1]) = 0xf3eb458161080000;
  *((unsigned long *)&__m128i_result[0]) = 0xffe9454286c0e000;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0051005200510052;
  *((unsigned long *)&__m128i_op1[0]) = 0x0051005200510052;
  *((unsigned long *)&__m128i_op2[1]) = 0xffaeffaeffaeffae;
  *((unsigned long *)&__m128i_op2[0]) = 0xffaeffaeffaeffae;
  *((unsigned long *)&__m128i_result[1]) = 0xffffe65ecc1be5bc;
  *((unsigned long *)&__m128i_result[0]) = 0xffffe65ecc1be5bc;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long *)&__m128i_op0[0]) = 0xf1f1f1f1865e65a1;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffff941d;
  *((unsigned long *)&__m128i_op2[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long *)&__m128i_op2[0]) = 0xf1f1f1f1865e65a1;
  *((unsigned long *)&__m128i_result[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long *)&__m128i_result[0]) = 0x78508ad4ec2ffcde;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x000000120000000d;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000cfffffff2;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000dfffffff1;
  __m128i_out = __lsx_vmaddwev_d_wu_w (__m128i_op0, __m128i_op1, __m128i_op2);
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
