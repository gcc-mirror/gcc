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
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000003004;
  *((unsigned long *)&__m128i_result[1]) = 0x0000400000004000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000400000007004;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfeffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfeffffffffffffff;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x38);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000001;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4000400040004000;
  *((unsigned long *)&__m128i_result[0]) = 0x4000400040004000;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000007fff8000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001008100000005;
  *((unsigned long *)&__m128i_result[1]) = 0x0800080077ff8800;
  *((unsigned long *)&__m128i_result[0]) = 0x0801088108000805;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0202020202020202;
  *((unsigned long *)&__m128i_result[0]) = 0x0202020202020202;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xe86ce7eb5e9ce950;
  *((unsigned long *)&__m128i_result[1]) = 0x0404040404040404;
  *((unsigned long *)&__m128i_result[0]) = 0xec68e3ef5a98ed54;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000200010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000400000004000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000400000204010;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0400040004000400;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffff02;
  *((unsigned long *)&__m128i_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_result[0]) = 0x04000400fbfffb02;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0010000000100000;
  *((unsigned long *)&__m128i_result[0]) = 0x0010000000100000;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_result[0]) = 0x040004000400040d;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000004f804f81;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000004f804f80;
  *((unsigned long *)&__m128i_result[1]) = 0x000000004fc04f81;
  *((unsigned long *)&__m128i_result[0]) = 0x000000004fc04f80;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0040004000400040;
  *((unsigned long *)&__m128i_result[0]) = 0x0040004000400040;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x1010101010101010;
  *((unsigned long *)&__m128i_result[0]) = 0xefefefefefefefef;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4040404040404040;
  *((unsigned long *)&__m128i_result[0]) = 0x4040404040404040;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x21201f1e1d1c1b1a;
  *((unsigned long *)&__m128i_op0[0]) = 0x1918171615141312;
  *((unsigned long *)&__m128i_result[1]) = 0x01203f1e3d1c3b1a;
  *((unsigned long *)&__m128i_result[0]) = 0x3918371635143312;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x61608654a2d4f6da;
  *((unsigned long *)&__m128i_result[1]) = 0xfff0800080008000;
  *((unsigned long *)&__m128i_result[0]) = 0xe160065422d476da;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x77c0401040004000;
  *((unsigned long *)&__m128i_result[0]) = 0x77c0401040004000;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x77c0404a4000403a;
  *((unsigned long *)&__m128i_op0[0]) = 0x77c03fd640003fc6;
  *((unsigned long *)&__m128i_result[1]) = 0x75c0404a4200403a;
  *((unsigned long *)&__m128i_result[0]) = 0x75c03fd642003fc6;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op0[0]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808280808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808280808;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffefffffffeff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000100fffffeff;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000010000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000010000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0404050404040404;
  *((unsigned long *)&__m128i_result[0]) = 0x0404050404040404;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[0]) = 0x1000100010001000;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xbfbfbfbfbfbfbfbf;
  *((unsigned long *)&__m128i_result[0]) = 0xbfbfbfbfbfbfbfbf;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000040000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000040000000;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000020000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000020000;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x2000200020002000;
  *((unsigned long *)&__m128i_result[0]) = 0x2000200020002000;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x441ba9fcffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x181b2541ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x401fadf8fbfbfbfb;
  *((unsigned long *)&__m128i_result[0]) = 0x1c1f2145fbfbfbfb;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000100;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffefff00001000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffefff00001000;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080808080808080;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000200000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000200000000;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x21);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000002000;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010000000100;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010000000100;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xd6a09e662ab46b31;
  *((unsigned long *)&__m128i_op0[0]) = 0x34b8122ef4054bb3;
  *((unsigned long *)&__m128i_result[1]) = 0xd6e09e262af46b71;
  *((unsigned long *)&__m128i_result[0]) = 0x34f8126ef4454bf3;
  __m128i_out = __lsx_vbitrevi_h (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000200008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000200000;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfefefefdbffefdfe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfefefeeffef7feff;
  *((unsigned long *)&__m128i_result[1]) = 0xfcfcfcffbdfcfffc;
  *((unsigned long *)&__m128i_result[0]) = 0xfcfcfcedfcf5fcfd;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000555889;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000002580f01;
  *((unsigned long *)&__m128i_result[1]) = 0x0010000000455889;
  *((unsigned long *)&__m128i_result[0]) = 0x0010000002480f01;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00060fbf00040fbf;
  *((unsigned long *)&__m128i_op0[0]) = 0x00020fbf00000fbf;
  *((unsigned long *)&__m128i_result[1]) = 0x00060fbf02040fbf;
  *((unsigned long *)&__m128i_result[0]) = 0x00020fbf02000fbf;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000007fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x400000003fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x4000000040000000;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x021b7d24c9678a35;
  *((unsigned long *)&__m128i_op0[0]) = 0x030298a6a1030a49;
  *((unsigned long *)&__m128i_result[1]) = 0x00197f26cb658837;
  *((unsigned long *)&__m128i_result[0]) = 0x01009aa4a301084b;
  __m128i_out = __lsx_vbitrevi_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x3);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000c6c60000c6c6;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000c6c58000c6b2;
  *((unsigned long *)&__m128i_result[1]) = 0x0000c6c40000c6c6;
  *((unsigned long *)&__m128i_result[0]) = 0x8000c6c78000c6b2;
  __m128i_out = __lsx_vbitrevi_d (__m128i_op0, 0x21);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffff7fffffff7f;
  *((unsigned long *)&__m128i_result[0]) = 0xffffff7fffffff7f;
  __m128i_out = __lsx_vbitrevi_w (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
