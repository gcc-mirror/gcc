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

  *((unsigned long *)&__m128i_op0[1]) = 0x0403cfcf01c1595e;
  *((unsigned long *)&__m128i_op0[0]) = 0x837cd5db43fc55d4;
  *((unsigned long *)&__m128i_op1[1]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op1[0]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_result[1]) = 0x0007005200440062;
  *((unsigned long *)&__m128i_result[0]) = 0x0080005e007f00d8;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffcafff8ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000000a0;
  *((unsigned long *)&__m128i_op1[1]) = 0xe6d4572c8a5835bc;
  *((unsigned long *)&__m128i_op1[0]) = 0xe5017c2ac9ca9fd0;
  *((unsigned long *)&__m128i_result[1]) = 0x00d3012b015700bb;
  *((unsigned long *)&__m128i_result[0]) = 0x0001002affca0070;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000fea0000fffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363771163631745;
  *((unsigned long *)&__m128i_op1[0]) = 0x636363ec6363636c;
  *((unsigned long *)&__m128i_result[1]) = 0x006300fb00630143;
  *((unsigned long *)&__m128i_result[0]) = 0x0063ffec0063006c;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x9c9c9c9c9c9c9c9d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffff0000;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8080ffffffff8080;
  *((unsigned long *)&__m128i_op1[0]) = 0x00008080ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xff80ffffffffff80;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff80ffffffff;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00197f26cb658837;
  *((unsigned long *)&__m128i_op0[0]) = 0x01009aa4a301084b;
  *((unsigned long *)&__m128i_op1[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op1[0]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_result[1]) = 0x0037ffd40083ffe5;
  *((unsigned long *)&__m128i_result[0]) = 0x001e0052001ffff9;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff00ffffff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000f50000000900;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000090900000998;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff000900ffff98;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f881a20ffd02b0;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ff800000;
  *((unsigned long *)&__m128i_result[1]) = 0xfff8ffa2fffdffb0;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ff800000;
  __m128i_out = __lsx_vaddwev_h_bu_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00007fff00007fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1e0200001e020000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffcfffcfffcfffd;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffcfffdfffcfffd;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffcfffffffd;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffdfffffffd;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff000000ff00;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000005;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010100000101;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010100000101;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000400000004;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000000080000008;
  *((unsigned long *)&__m128i_op1[0]) = 0xa2f54a1ea2f54a1e;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x00004a1e00004a1e;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000868686868686;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000868600008785;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x63636b6afe486741;
  *((unsigned long *)&__m128i_op0[0]) = 0x41f8e880ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xe363636363abdf16;
  *((unsigned long *)&__m128i_op1[0]) = 0x41f8e08016161198;
  *((unsigned long *)&__m128i_result[1]) = 0x0000cecd00004657;
  *((unsigned long *)&__m128i_result[0]) = 0x0000c90000011197;
  __m128i_out = __lsx_vaddwev_w_hu_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001000f000e;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000fff1000ffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000f000e;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000ffffe;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0c07e181ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x3430af9effffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000fe00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000fe00ff;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00060012000e002b;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000049ffffffaa;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000e002b;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffaa;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000958affff995d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000bfffffffe0f6;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000001f0a;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffff7a53;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff7f80ffff7f80;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff7f80ffff7f80;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff7f80ffff7f80;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff7f80ffff7f80;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000fffeff00;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000fffeff00;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0008000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000003dffc2;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000003dffc2;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0080008000800080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0080006b0000000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000800080;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000ff00ff;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000455555555;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000055555555;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff7f810100001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000400530050ffa6;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff007fff810001;
  *((unsigned long *)&__m128i_op1[0]) = 0x000400530050ffa6;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffff811001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000a1ff4c;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000001f;
  *((unsigned long *)&__m128i_result[0]) = 0x000000008000001e;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xd3259a2984048c23;
  *((unsigned long *)&__m128i_op0[0]) = 0xf9796558e39953fd;
  *((unsigned long *)&__m128i_op1[1]) = 0x86dd8341b164f12b;
  *((unsigned long *)&__m128i_op1[0]) = 0x9611c3985b3159f5;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000035697d4e;
  *((unsigned long *)&__m128i_result[0]) = 0x000000013ecaadf2;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ef00ff010f;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff010f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0xc1f03e1042208410;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000001000110;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000431f851f;
  __m128i_out = __lsx_vaddwev_d_wu_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000030000003f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000030000003f;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffbfffffffbe;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x06b1213ef1efa299;
  *((unsigned long *)&__m128i_op0[0]) = 0x8312f5424ca4a07f;
  *((unsigned long *)&__m128i_op1[1]) = 0x1f1f1f1f1f1f1f00;
  *((unsigned long *)&__m128i_op1[0]) = 0x1f1f1f27332b9f00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xa23214697fd03f7f;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x80000000ffffd860;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff80000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff80000000;
  __m128i_out = __lsx_vaddwev_q_du_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
