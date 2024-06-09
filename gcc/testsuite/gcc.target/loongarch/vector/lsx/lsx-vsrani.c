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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000201000000000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0005000501800005;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x438ff81ff81ff820;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x03ff03ff03ff03ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000043;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000003;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0x78);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000002020202;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0x5b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000009;
  *((unsigned long *)&__m128i_op1[1]) = 0x697eba2bedfa9c82;
  *((unsigned long *)&__m128i_op1[0]) = 0xd705c77a7025c899;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x03fdfffcfefe03fe;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0100000001000100;
  *((unsigned long *)&__m128i_op0[0]) = 0x0100010000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ffffff00ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff00ffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010001000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff00ff00ffffff;
  __m128i_out = __lsx_vsrani_h_w (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x40f0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x40f0001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1e0200001e020000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_op1[0]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0040004000400040;
  __m128i_out = __lsx_vsrani_w_d (__m128i_op0, __m128i_op1, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000040000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001ffce00016fb41;
  *((unsigned long *)&__m128i_op0[0]) = 0x57cb857100001a46;
  *((unsigned long *)&__m128i_op1[1]) = 0xfbffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x7bffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000150000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffeffff001effff;
  __m128i_out = __lsx_vsrani_h_w (__m128i_op0, __m128i_op1, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x1);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_op1[0]) = 0x2020202020207fff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x01010101010101ff;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff082f000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x003f000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff000000000000;
  __m128i_out = __lsx_vsrani_h_w (__m128i_op0, __m128i_op1, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_h_w (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00005dcbe7e830c0;
  *((unsigned long *)&__m128i_op0[0]) = 0x03f21e0114bf19da;
  *((unsigned long *)&__m128i_op1[1]) = 0x000003f200001e01;
  *((unsigned long *)&__m128i_op1[0]) = 0x000014bf000019da;
  *((unsigned long *)&__m128i_result[1]) = 0x0005fe0300010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100010001;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x62cbf96e4acfaf40;
  *((unsigned long *)&__m128i_op1[0]) = 0xf0bc9a5278285a4a;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[0]) = 0x62cbf96e4acfaf40;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0x40);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f5533a694f902c0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1f54e0ab00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffb6d01f5f94f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001f50000;
  __m128i_out = __lsx_vsrani_h_w (__m128i_op0, __m128i_op1, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_w_d (__m128i_op0, __m128i_op1, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x808080e280808080;
  *((unsigned long *)&__m128i_op1[0]) = 0x8080636380806363;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8080808080638063;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000001d;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000001d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0x63);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0f07697100000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000076971000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_w_d (__m128i_op0, __m128i_op1, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000003020302;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffff81;
  *((unsigned long *)&__m128i_result[1]) = 0x00000c0c00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffe;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_d_q (__m128i_op0, __m128i_op1, 0x58);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vsrani_w_d (__m128i_op0, __m128i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrani_b_h (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5847b72626ce61ef;
  *((unsigned long *)&__m128i_op0[0]) = 0x110053f401e7cced;
  *((unsigned long *)&__m128i_op1[1]) = 0x5847b72626ce61ef;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0005847b00011005;
  *((unsigned long *)&__m128i_result[0]) = 0x0005847b00000000;
  __m128i_out = __lsx_vsrani_w_d (__m128i_op0, __m128i_op1, 0x2c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
