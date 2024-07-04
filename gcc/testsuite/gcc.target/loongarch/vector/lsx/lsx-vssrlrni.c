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
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x3d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8080808000008080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080000080800000;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000080000000;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000017fff9000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000210011084;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000007f0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100000004;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000007f00;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0101010400100203;
  *((unsigned long *)&__m128i_op0[0]) = 0x0103010301020109;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000110000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000007f00000004;
  *((unsigned long *)&__m128i_result[1]) = 0x0202000402020202;
  *((unsigned long *)&__m128i_result[0]) = 0x0000200000010000;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x56);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x6d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0001ffff8002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0010000400020004;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ffff20ff;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc0020ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x07fff80000008000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000007ffe001;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_d_q (__m128i_op0, __m128i_op1, 0x7c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x03574e3b94f2ca31;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000001f807b89;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000005050000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0505000005050505;
  *((unsigned long *)&__m128i_result[1]) = 0x000d02540000007e;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001400140014;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x41);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x3b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x56a09e662ab46b31;
  *((unsigned long *)&__m128i_op1[0]) = 0xb4b8122ef4054bb3;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x02b504f305a5c091;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x37);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000d000d000d000d;
  *((unsigned long *)&__m128i_op0[0]) = 0x000d000d000d000d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000680000006800;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000400;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00005555aaabfffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x003fffffff000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000000ab;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000000ff;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x43);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff000000ff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000007fff7fff;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007fff7fff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff0000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000080;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000080000000000;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x34);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x027c027c000027c0;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000004f804f81;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000004f804f80;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000010000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001400000014;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ff81007c;
  *((unsigned long *)&__m128i_op0[0]) = 0xffb7005f0070007c;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff80007e028401;
  *((unsigned long *)&__m128i_op1[0]) = 0x9a10144000400000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001ffff00010;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x5b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x29);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000040000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000080000000000;
  __m128i_out = __lsx_vssrlrni_hu_w (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffff9cff05;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffff9cfebd;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000002;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op1[1]) = 0xff7ffffef77fffdd;
  *((unsigned long *)&__m128i_op1[0]) = 0xf77edf9cffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff7fffffff;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001fffff001fffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x001fffff001fffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x21201f1e1d1c1b1a;
  *((unsigned long *)&__m128i_op1[0]) = 0x1918171615141312;
  *((unsigned long *)&__m128i_result[1]) = 0x10ff10ff10ff10ff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffa6ff91fdd8ef77;
  *((unsigned long *)&__m128i_op0[0]) = 0x061202bffb141c38;
  *((unsigned long *)&__m128i_op1[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[0]) = 0x010101fe0101fe87;
  *((unsigned long *)&__m128i_result[1]) = 0x0000004000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffd60001723aa5f8;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000007f007f7f;
  *((unsigned long *)&__m128i_result[0]) = 0x7f7f7f7f7f7f7f7f;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x808080e280808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080636380806363;
  *((unsigned long *)&__m128i_op1[1]) = 0x808080e280808080;
  *((unsigned long *)&__m128i_op1[0]) = 0x8080636380806363;
  *((unsigned long *)&__m128i_result[1]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_result[0]) = 0x0004000400040004;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000d0000000d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000dffff000d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000070007;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000007ffff;
  __m128i_out = __lsx_vssrlrni_hu_w (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000800c00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_hu_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000007fff7fff;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffff0100ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0607060700000807;
  *((unsigned long *)&__m128i_op1[0]) = 0x0707f8f803e8157e;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x31);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x21);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5252525252525252;
  *((unsigned long *)&__m128i_op0[0]) = 0x5252525252525252;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc0808000c0808000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000003020302;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffc0ff80ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_wu_d (__m128i_op0, __m128i_op1, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffff0000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffc0800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000008080600;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x52525252adadadad;
  *((unsigned long *)&__m128i_op0[0]) = 0x52525252adadadad;
  *((unsigned long *)&__m128i_op1[1]) = 0x800000007fffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x800000007fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlrni_hu_w (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x003ef89df07f0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x003ec0fc0fbfe001;
  *((unsigned long *)&__m128i_op1[1]) = 0x3ff800ff2fe6c00d;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff40408ece0e0de;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff7fffffff;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4000400040004000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ff960001005b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffa500010003;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0020000000000000;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x2b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1748c4f9ed1a5870;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffffffffffff;
  __m128i_out = __lsx_vssrlrni_d_q (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_op1[0]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000000010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vssrlrni_hu_w (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4000000040000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x27);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0x28);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_du_q (__m128i_op0, __m128i_op1, 0x26);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x117d7f7b093d187f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000034;
  *((unsigned long *)&__m128i_op1[1]) = 0xfe1bfefe00011ee1;
  *((unsigned long *)&__m128i_op1[0]) = 0xfe1bfe6c03824c60;
  *((unsigned long *)&__m128i_result[1]) = 0x7f7f7f7f0000001a;
  *((unsigned long *)&__m128i_result[0]) = 0x7f7f017f7f7f7f7f;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff3a81ffff89fd;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffb3c3ffff51ba;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0802080408060803;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff00ffffff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff000900ffff98;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff7fffffff;
  __m128i_out = __lsx_vssrlrni_w_d (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0xc);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000056000056;
  *((unsigned long *)&__m128i_op0[0]) = 0x3a8000003a800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000efffefff;
  *((unsigned long *)&__m128i_op1[0]) = 0xa03aa03ae3e2e3e2;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_d_q (__m128i_op0, __m128i_op1, 0x75);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000760151;
  *((unsigned long *)&__m128i_op0[0]) = 0x003e0021009a009a;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000246d9755;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000003e2427c2ee;
  *((unsigned long *)&__m128i_result[1]) = 0x00001e5410082727;
  *((unsigned long *)&__m128i_result[0]) = 0x00007f7f00107f7f;
  __m128i_out = __lsx_vssrlrni_b_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000f1384;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000004ff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vssrlrni_bu_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f8000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlrni_h_w (__m128i_op0, __m128i_op1, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
