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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x1414141414141415;
  *((unsigned long *)&__m128i_result[0]) = 0x1414141414141415;
  __m128i_out = __lsx_vaddi_bu (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0505050505050505;
  *((unsigned long *)&__m128i_result[0]) = 0x0505050504040404;
  __m128i_out = __lsx_vaddi_bu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000008140c80;
  *((unsigned long *)&__m128i_result[1]) = 0x1f1f1f1f1f1f1f1f;
  *((unsigned long *)&__m128i_result[0]) = 0x1f1f1f1f27332b9f;
  __m128i_out = __lsx_vaddi_bu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_result[0]) = 0x0303030303030304;
  __m128i_out = __lsx_vaddi_bu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[1]) = 0x8f8f8f8f8f8f8f8f;
  *((unsigned long *)&__m128i_result[0]) = 0x8f8f8f8f8f8f8f8f;
  __m128i_out = __lsx_vaddi_bu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0018001800180018;
  *((unsigned long *)&__m128i_result[0]) = 0x0018001800180018;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000080000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000080000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0019081900190019;
  *((unsigned long *)&__m128i_result[0]) = 0x0019081900190019;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_result[0]) = 0x000a000a000a000a;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffc1000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000ff0000;
  *((unsigned long *)&__m128i_result[1]) = 0xffcc000b000b000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000b000b010a000b;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001f001f001f001f;
  *((unsigned long *)&__m128i_result[0]) = 0x001f001f001f001f;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001c001c001c001c;
  *((unsigned long *)&__m128i_result[0]) = 0x001c001c001c001c;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x67eb85afb2ebb000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128i_result[1]) = 0x680485c8b304b019;
  *((unsigned long *)&__m128i_result[0]) = 0xc89d7f0fed582019;
  __m128i_out = __lsx_vaddi_hu (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000a0000000a;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000a0000000a;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffe000ffff1fff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000090100000a;
  *((unsigned long *)&__m128i_result[0]) = 0xffffe009ffff2008;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000300000003;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfc01fcfefc02fdf7;
  *((unsigned long *)&__m128i_op0[0]) = 0xfe00fcfffe01fd01;
  *((unsigned long *)&__m128i_result[1]) = 0xfc01fd13fc02fe0c;
  *((unsigned long *)&__m128i_result[0]) = 0xfe00fd14fe01fd16;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001300000013;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000bd3d;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007fff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000c0000bd49;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000c7fff000c;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fffe0001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000500000005;
  *((unsigned long *)&__m128i_result[0]) = 0x00000005fffe0006;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000fffffeff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000009ffffff08;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000900000009;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x55aa55aa55aa55ab;
  *((unsigned long *)&__m128i_op0[0]) = 0xaa55555655aaaaa8;
  *((unsigned long *)&__m128i_result[1]) = 0x55aa55c355aa55c4;
  *((unsigned long *)&__m128i_result[0]) = 0xaa55556f55aaaac1;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000e0000002e;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000e0000004e;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000400000004;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x003f000400000003;
  *((unsigned long *)&__m128i_result[0]) = 0x003f000400000003;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff8000010f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000900000009;
  *((unsigned long *)&__m128i_result[0]) = 0xff80000a0f800009;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x30eb020302101b03;
  *((unsigned long *)&__m128i_op0[0]) = 0x020310d0c0030220;
  *((unsigned long *)&__m128i_result[1]) = 0x30eb022002101b20;
  *((unsigned long *)&__m128i_result[0]) = 0x020310edc003023d;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x59f7fd7059f7fd70;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001700000017;
  *((unsigned long *)&__m128i_result[0]) = 0x59f7fd8759f7fd87;
  __m128i_out = __lsx_vaddi_wu (__m128i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6420e0208400c4c4;
  *((unsigned long *)&__m128i_op0[0]) = 0x20c4e0c4e0da647a;
  *((unsigned long *)&__m128i_result[1]) = 0x6420e0208400c4e3;
  *((unsigned long *)&__m128i_result[0]) = 0x20c4e0c4e0da6499;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x21201f1e1d001b1a;
  *((unsigned long *)&__m128i_op0[0]) = 0x1918171615141312;
  *((unsigned long *)&__m128i_result[1]) = 0x21201f1e1d001b25;
  *((unsigned long *)&__m128i_result[0]) = 0x191817161514131d;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000014;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000004000000040;
  *((unsigned long *)&__m128i_op0[0]) = 0x00007770ffff9411;
  *((unsigned long *)&__m128i_result[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_result[0]) = 0x00007770ffff941d;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000016;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000016;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000080000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vaddi_du (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
