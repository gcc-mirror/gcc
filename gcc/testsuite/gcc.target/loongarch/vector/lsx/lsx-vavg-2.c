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
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x7f7f7f7f7f7f7f7f;
  *((unsigned long *)&__m128i_result[0]) = 0x7f7f7f7f7f7f7f7f;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[0]) = 0x7f7f7f7f7f7f7f7f;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7f7f7f7f7f7f7f7f;
  *((unsigned long *)&__m128i_result[0]) = 0x7f7f7f7f7f7f7f7f;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000100000001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x37b951002d81a921;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_op1[0]) = 0x000047404f4f040d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000082000000826;
  *((unsigned long *)&__m128i_result[0]) = 0x1b5c4c203e685617;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128i_op0[0]) = 0x00a975be00accf03;
  *((unsigned long *)&__m128i_op1[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128i_op1[0]) = 0x00a975be00accf03;
  *((unsigned long *)&__m128i_result[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128i_result[0]) = 0x00a975be00accf03;
  __m128i_out = __lsx_vavg_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0100000001000100;
  *((unsigned long *)&__m128i_op0[0]) = 0x0100010000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffff732a;
  *((unsigned long *)&__m128i_result[1]) = 0x807f7fff807f807f;
  *((unsigned long *)&__m128i_result[0]) = 0x807f807f7fff3995;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000007f7f7f7f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffff0;
  *((unsigned long *)&__m128i_result[1]) = 0x000000003fbf3fbf;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff7fff7fff7ff8;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op0[0]) = 0x353c8cc4b1ec5b09;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff00000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8080008000808080;
  *((unsigned long *)&__m128i_result[0]) = 0x1a9e466258f62d84;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000158;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000000ac;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x9c9c9c9c00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[1]) = 0x4e4e4e4e00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000868686868686;
  *((unsigned long *)&__m128i_op1[1]) = 0x1e1e1e1e1e1e1e1e;
  *((unsigned long *)&__m128i_op1[0]) = 0x1e1e1e1e1e1e1e1e;
  *((unsigned long *)&__m128i_result[1]) = 0x0f0f0f0f0f0f0f0f;
  *((unsigned long *)&__m128i_result[0]) = 0x0f0f525252525252;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000014eb54ab;
  *((unsigned long *)&__m128i_op0[0]) = 0x14eb6a002a406a00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffdfdc0d;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000a752a55;
  *((unsigned long *)&__m128i_result[0]) = 0x0a753500950fa306;
  __m128i_out = __lsx_vavg_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffff00010000fff;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000002ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000017fffffff;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0101000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0101030100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0080800000008000;
  *((unsigned long *)&__m128i_result[0]) = 0x0080818000008000;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000200000002;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0017004800c400f9;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ed001a00580070;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x800b7fe38062007b;
  *((unsigned long *)&__m128i_result[0]) = 0x0076800d802c0037;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff80ffa2fff0ff74;
  *((unsigned long *)&__m128i_op0[0]) = 0xff76ffd8ffe6ffaa;
  *((unsigned long *)&__m128i_op1[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128i_op1[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128i_result[1]) = 0xe01ae8a3fc55dd23;
  *((unsigned long *)&__m128i_result[0]) = 0xdd9ff64ef9daeace;
  __m128i_out = __lsx_vavg_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000007fffffff;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3f80000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3f80000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x1fc0000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1fc07f8000007f80;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000043cf26c7;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000e31d4cae8636;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000021e79364;
  *((unsigned long *)&__m128i_result[0]) = 0x0000718ea657431b;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7ff8000000000000;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffff80ffff7e02;
  *((unsigned long *)&__m128i_op0[0]) = 0x00feff8000ff80ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xf931fd04f832fe02;
  *((unsigned long *)&__m128i_result[1]) = 0x80007fc000003f00;
  *((unsigned long *)&__m128i_result[0]) = 0x7d187e427c993f80;
  __m128i_out = __lsx_vavg_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
