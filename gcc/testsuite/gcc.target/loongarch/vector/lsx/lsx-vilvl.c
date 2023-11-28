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
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_op1[0]) = 0x000201000000000b;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000201000000000b;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffefffffffc;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffefffffffeff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffcff;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7404443064403aec;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000d6eefefc0498;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff7f800000;
  *((unsigned long *)&__m128i_op1[0]) = 0x2d1da85b7f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x002d001dd6a8ee5b;
  *((unsigned long *)&__m128i_result[0]) = 0xfe7ffc8004009800;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000800;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001000000010;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000c0000bd49;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000c7fff000c;
  *((unsigned long *)&__m128i_op1[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000c7fff000c;
  *((unsigned long *)&__m128i_result[0]) = 0x1000100010001000;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff0000007f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001e8e1d8;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000e400000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000001e8e1d8;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000e400000001;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000e4e4;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000101;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0008000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0008000000000000;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffe0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff0000ffe0;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xbafebb00ffd500fe;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x80808080806b000b;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff80005613;
  *((unsigned long *)&__m128i_op1[0]) = 0x007f800000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8000807f80808000;
  *((unsigned long *)&__m128i_result[0]) = 0x80006b0000000b00;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000080808000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x80808080806b000b;
  *((unsigned long *)&__m128i_result[1]) = 0x0080008000800080;
  *((unsigned long *)&__m128i_result[0]) = 0x0080006b0000000b;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc0808000c0808000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xc080800000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xc080800000000000;
  __m128i_out = __lsx_vilvl_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff010300ff0103;
  *((unsigned long *)&__m128i_op1[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x007ffff001000300;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff0001000300;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x007f00ff00ff00fe;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0x8);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0014001400140000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001400000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001400000000;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00009c7c00007176;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000009c007c00;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000071007600;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x9c9c9c9c00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000060002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000060002;
  *((unsigned long *)&__m128i_op1[1]) = 0xe4c8b96e2560afe9;
  *((unsigned long *)&__m128i_op1[0]) = 0xc001a1867fffa207;
  *((unsigned long *)&__m128i_result[1]) = 0x0000c0010000a186;
  *((unsigned long *)&__m128i_result[0]) = 0x00067fff0002a207;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000014414104505;
  *((unsigned long *)&__m128i_op0[0]) = 0x1011050040004101;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000014414104505;
  *((unsigned long *)&__m128i_op1[0]) = 0x1011050040004101;
  *((unsigned long *)&__m128i_result[1]) = 0x1010111105050000;
  *((unsigned long *)&__m128i_result[0]) = 0x4040000041410101;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vilvl_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffac5cffffac5c;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffac5cffffac5c;
  *((unsigned long *)&__m128i_op1[1]) = 0x010169d9010169d9;
  *((unsigned long *)&__m128i_op1[0]) = 0x01010287010146a1;
  *((unsigned long *)&__m128i_result[1]) = 0xff01ff01ac025c87;
  *((unsigned long *)&__m128i_result[0]) = 0xff01ff01ac465ca1;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff01ff01ac025c87;
  *((unsigned long *)&__m128i_op0[0]) = 0xff01ff01ac465ca1;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff01ff0100000000;
  *((unsigned long *)&__m128i_result[0]) = 0xac465ca100000000;
  __m128i_out = __lsx_vilvl_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000eefff;
  *((unsigned long *)&__m128i_op0[0]) = 0xf8e1a03affffe3e2;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000246d9755;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002427c2ee;
  *((unsigned long *)&__m128i_result[1]) = 0xf8e10000a03a0000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff2427e3e2c2ee;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffe4ffe4ffe4ffe4;
  *((unsigned long *)&__m128i_op0[0]) = 0xffe4ffe4ffe4ffe4;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000011ff040;
  *((unsigned long *)&__m128i_result[1]) = 0xff00e400ff00e400;
  *((unsigned long *)&__m128i_result[0]) = 0xff01e41ffff0e440;
  __m128i_out = __lsx_vilvl_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff00e400ff00e400;
  *((unsigned long *)&__m128i_op0[0]) = 0xff01e41ffff0ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff01ffffe41f0000;
  *((unsigned long *)&__m128i_result[0]) = 0xfff00000ffff0000;
  __m128i_out = __lsx_vilvl_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
