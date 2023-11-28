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
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000010000000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000008000000080;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000c7fff000c;
  *((unsigned long *)&__m128i_op1[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op2[1]) = 0xfffff000f0008d3c;
  *((unsigned long *)&__m128i_op2[0]) = 0xfffff0016fff8d3d;
  *((unsigned long *)&__m128i_result[1]) = 0x00000100f8100002;
  *((unsigned long *)&__m128i_result[0]) = 0xfff0ff8006f0f950;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x007ffd0001400840;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x007ffd0001400840;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op2[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000002000;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010058;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010058;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffac0a000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000200000001b;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffac0a000000;
  __m128i_out = __lsx_vmaddwod_h_b (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000017fda829;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000017fda829;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xff8000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000800000000ffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x697eba2bedfa9c82;
  *((unsigned long *)&__m128i_op2[0]) = 0xd705c77a7025c899;
  *((unsigned long *)&__m128i_result[1]) = 0xffcb410000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffeb827ffffffff;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fffffc00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000200010;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000fffffc00;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000f;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000c5ac01015b;
  *((unsigned long *)&__m128i_op1[0]) = 0xaaacac88a3a9a96a;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000f;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ef4002d21fc7001;
  *((unsigned long *)&__m128i_op0[0]) = 0x28bf02d1ec6a35b2;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffb96bffff57c9;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff6080ffff4417;
  *((unsigned long *)&__m128i_op2[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op2[0]) = 0xff8000007fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x7ef400ad21fc7081;
  *((unsigned long *)&__m128i_result[0]) = 0x28bf0351ec69b5f2;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001200100012001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_w_h (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[1]) = 0xbf8000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xcf00000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xbf80000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0xcf00000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1040400000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0961000100000001;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x03574e3a62407e03;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001010000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7da9b23a624082fd;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x03574e39e496cbc9;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001010000;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffff01018888;
  *((unsigned long *)&__m128i_result[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000030000003f;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x3f77aab500000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffc100010001;
  *((unsigned long *)&__m128i_op2[1]) = 0x3f77aab500000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000ffc100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0fbc1df53c1ae3f9;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ff820f81;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000ff801c9e;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000810000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000700000004e000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0003000000012020;
  *((unsigned long *)&__m128i_op1[1]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_op2[1]) = 0x00000000e00a18f5;
  *((unsigned long *)&__m128i_op2[0]) = 0x000000002023dcdc;
  *((unsigned long *)&__m128i_result[1]) = 0x000700000004e000;
  *((unsigned long *)&__m128i_result[0]) = 0x0003000000012020;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000120000000d;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000011ffee;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000dfff2;
  __m128i_out = __lsx_vmaddwod_d_w (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff7fffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffff8000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffff7fffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffff8000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000003fff;
  *((unsigned long *)&__m128i_result[0]) = 0x7ff8010000000001;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ff0000ff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x01fc020000fe0100;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x78c00000ff000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x78c00000ff000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x78c00000ff000000;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_op1[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x1000100012030e02;
  *((unsigned long *)&__m128i_result[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m128i_result[0]) = 0xfefefefefefefefe;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000080800000808;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000080800000808;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x010105017878f8f6;
  *((unsigned long *)&__m128i_op2[0]) = 0xf8f8fd0180810907;
  *((unsigned long *)&__m128i_result[1]) = 0x0000080800000808;
  *((unsigned long *)&__m128i_result[0]) = 0x0000080800000808;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000158;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010058;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000158;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x328e1080889415a0;
  *((unsigned long *)&__m128i_op0[0]) = 0x3960b1a401811060;
  *((unsigned long *)&__m128i_op1[1]) = 0x328e1080889415a0;
  *((unsigned long *)&__m128i_op1[0]) = 0x3960b1a401811060;
  *((unsigned long *)&__m128i_op2[1]) = 0x020310edc003023d;
  *((unsigned long *)&__m128i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x32f3c7a38f9f4b8b;
  *((unsigned long *)&__m128i_result[0]) = 0x2c9e5069f5d57780;
  __m128i_out = __lsx_vmaddwod_q_d (__m128i_op0, __m128i_op1, __m128i_op2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
