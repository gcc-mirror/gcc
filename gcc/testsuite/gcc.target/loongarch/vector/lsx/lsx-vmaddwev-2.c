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
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff00000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x7fffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7e00fe0000000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op0[0]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffbffffff85;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffc0000fdfc;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3941248880000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x3941248880000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x40f3fa0000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x76f4248880000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000e36400005253;
  *((unsigned long *)&__m128i_op2[0]) = 0x000035ed0000e000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0080008000800080;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x400000003fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x4000000040000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000007fffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x400000003fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x4000000040000000;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long *)&__m128i_op0[0]) = 0x78508ad4ec2ffcde;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffdfdc0d;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x00000000ffdfdc0d;
  *((unsigned long *)&__m128i_result[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long *)&__m128i_result[0]) = 0x78508ad4ae70fd87;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000440efffff000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000003b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x440ef000440ef000;
  *((unsigned long *)&__m128i_op2[0]) = 0x4400000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000440efffff000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000003b;
  __m128i_out = __lsx_vmaddwev_h_bu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000ffc2f;
  *((unsigned long *)&__m128i_op0[0]) = 0x00201df000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffc2ffe700000007;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffc100010001;
  *((unsigned long *)&__m128i_op2[1]) = 0x00010020fffeffde;
  *((unsigned long *)&__m128i_op2[0]) = 0x011f57c100201a46;
  *((unsigned long *)&__m128i_result[1]) = 0x001ffce00016fb41;
  *((unsigned long *)&__m128i_result[0]) = 0x57cb857100001a46;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7c7c9c0000007176;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x00ff000000001f1f;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7c7c9c0000007176;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xbfd10d0d7b6b6b73;
  *((unsigned long *)&__m128i_op0[0]) = 0xc5c53492f25acbf2;
  *((unsigned long *)&__m128i_op1[1]) = 0xff000000001f1f00;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xbfd10d0d7b6b6b73;
  *((unsigned long *)&__m128i_result[0]) = 0xc5c53492f25acbf2;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x34947b4b11684f92;
  *((unsigned long *)&__m128i_op0[0]) = 0xd73691661e5b68b4;
  *((unsigned long *)&__m128i_op1[1]) = 0x000016f303dff6d2;
  *((unsigned long *)&__m128i_op1[0]) = 0x000016f303dff6d2;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x7fffffff00000001;
  *((unsigned long *)&__m128i_result[1]) = 0x34947b4b11684f92;
  *((unsigned long *)&__m128i_result[0]) = 0xee297a731e5c5f86;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0177fff0fffffff0;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000011ff8bc;
  *((unsigned long *)&__m128i_op2[1]) = 0x00bbfff7fffffff7;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffff008ff820;
  *((unsigned long *)&__m128i_result[1]) = 0xffe8008fffe7008f;
  *((unsigned long *)&__m128i_result[0]) = 0x00010001f1153780;
  __m128i_out = __lsx_vmaddwev_w_hu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100000021;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010002;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op2[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op2[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[1]) = 0x0001808281820102;
  *((unsigned long *)&__m128i_result[0]) = 0x0001808201018081;
  __m128i_out = __lsx_vmaddwev_d_wu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000010100fe0101;
  *((unsigned long *)&__m128i_op2[0]) = 0xffff0200ffff01ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_d_wu (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000017fda829;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_op2[1]) = 0x00ffff0000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x00ffff0000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x007f7f80807f7f80;
  *((unsigned long *)&__m128i_result[0]) = 0x8000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000020302030;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000020302030;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x3fffffffc0000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000006e17bfd8;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000006e17bfd8;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffff0100000001;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffff0100000001;
  *((unsigned long *)&__m128i_result[1]) = 0x000000006e17bfd8;
  *((unsigned long *)&__m128i_result[0]) = 0x000000006e17bfd8;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op2[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op2[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x00010000fffffffc;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000004;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000008000e2e3;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000008000e2e3;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000080806362;
  *((unsigned long *)&__m128i_result[0]) = 0x807f808000000000;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffa;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8101010181010101;
  *((unsigned long *)&__m128i_op1[0]) = 0x8101010181010101;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000101010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000101010015;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffed00010001;
  __m128i_out = __lsx_vmaddwev_q_du (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
