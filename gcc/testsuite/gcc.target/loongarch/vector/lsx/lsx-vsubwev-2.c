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
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ff00ff00000083;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_result[1]) = 0xff01ff010000ff7d;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000fffc;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff00fc0000ff02;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0xff01ff040000fffe;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f5533a694f902c0;
  *((unsigned long *)&__m128i_op1[1]) = 0x21011f3f193d173b;
  *((unsigned long *)&__m128i_op1[0]) = 0xff39ff37ff35ff33;
  *((unsigned long *)&__m128i_result[1]) = 0x00fe008e009e0071;
  *((unsigned long *)&__m128i_result[0]) = 0x001c006f00c4008d;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x9c9ca19d509ae734;
  *((unsigned long *)&__m128i_op0[0]) = 0xd1b09480f2123460;
  *((unsigned long *)&__m128i_op1[1]) = 0x9c9c9c9c9c9c9c9c;
  *((unsigned long *)&__m128i_op1[0]) = 0x9c9c9c9c9c9c9c9c;
  *((unsigned long *)&__m128i_result[1]) = 0x00000001fffeff98;
  *((unsigned long *)&__m128i_result[0]) = 0x0014ffe4ff76ffc4;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x34947b4b11684f92;
  *((unsigned long *)&__m128i_op1[0]) = 0xee297a731e5c5f86;
  *((unsigned long *)&__m128i_result[1]) = 0xff6cffb5ff98ff6e;
  *((unsigned long *)&__m128i_result[0]) = 0xffd7ff8dffa4ff7a;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_h_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff01018888;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff3ea5016b;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffefffe3f6fb04d;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000d96f;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001ffffd83b;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000f0009d3c;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000016fff9d3d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000bd0;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000000007f0;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000916c;
  *((unsigned long *)&__m128i_result[0]) = 0x000000010000954d;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000100010000fe01;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000050000007b;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000500000005;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffbffffff85;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffc0000fdfc;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000032;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000032;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff80df00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xa5c4c774856ba837;
  *((unsigned long *)&__m128i_op1[0]) = 0x2a569f8081c3bbe9;
  *((unsigned long *)&__m128i_result[1]) = 0xffffb96bffff57c9;
  *((unsigned long *)&__m128i_result[0]) = 0xffff6080ffff4417;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000063b2ac27;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffaa076aeb;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffff53d9;
  *((unsigned long *)&__m128i_result[0]) = 0xffff0001ffff9515;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff0000ffff;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00060fbf00040fbf;
  *((unsigned long *)&__m128i_op0[0]) = 0x00020fbf00000fbf;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[1]) = 0xffffac5cffffac5c;
  *((unsigned long *)&__m128i_result[0]) = 0xffffac5cffffac5c;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffaefffbffaefffb;
  *((unsigned long *)&__m128i_op1[0]) = 0xffaefffbffaefffb;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0005ffff0005;
  *((unsigned long *)&__m128i_result[0]) = 0xffff000500000004;
  __m128i_out = __lsx_vsubwev_w_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000a1630000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000a1630000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000001fd0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000001fd0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff7ffffef77fffdd;
  *((unsigned long *)&__m128i_op1[0]) = 0xf77edf9cffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000008800022;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000001;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffda6f;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffe3d7;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffda6e;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffe3d6;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x003fffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x003fffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8000807f00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x80006b0080808080;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff00011cf0c569;
  *((unsigned long *)&__m128i_op1[0]) = 0xc0000002b0995850;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffe30f3a97;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffcfe72830;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ff9f5c25;
  *((unsigned long *)&__m128i_op0[0]) = 0x58fa6b4000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ff9f5c25;
  *((unsigned long *)&__m128i_op1[0]) = 0x58fa6b4000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xcda585aebbb2836a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000080808080;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffc4cdfd16;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  *((unsigned long *)&__m128i_op0[1]) = 0x801dd5cb0004e058;
  *((unsigned long *)&__m128i_op0[0]) = 0x77eb15638eeb5fc2;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000200000001b;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000004e03d;
  *((unsigned long *)&__m128i_result[0]) = 0x000000008eeb5fc2;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_d_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000c0000bd49;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000c7fff000c;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffff00010000fff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000100c6ffef00d;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000006f00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000c00000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000006f;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000001f0a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000bfffffffe0f6;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffcfffcfffcfffd;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffcfffdfffcfffd;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffcfffdfffcfffd;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffff7e00000081;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffb96bffff57c9;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff6080ffff4417;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0aa9890a0ac5f3;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x370bdfecffecffec;
  *((unsigned long *)&__m128i_op0[0]) = 0x370bdfecffecffec;
  *((unsigned long *)&__m128i_op1[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x36fbdfdcffdcffdc;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000000;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffefffefffeffff;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000a752a55;
  *((unsigned long *)&__m128i_op1[0]) = 0x0a753500a9fa0d06;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xf589caff5605f2fa;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x087c000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000087c;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f8000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000001000010f8;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffefffff784;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000000000000;
  __m128i_out = __lsx_vsubwev_q_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
