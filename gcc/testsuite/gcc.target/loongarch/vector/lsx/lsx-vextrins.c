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
  *((unsigned long *)&__m128i_op1[1]) = 0xbf8000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xcf00000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff00000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x92);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xc2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x3d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0200020002000200;
  *((unsigned long *)&__m128i_op1[0]) = 0x0200020002000200;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff02000200;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0c03e17edd781b11;
  *((unsigned long *)&__m128i_op0[0]) = 0x342caf9be55700b5;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000040400000383;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffe000ffff1fff;
  *((unsigned long *)&__m128i_result[1]) = 0x0c03e17edd781b11;
  *((unsigned long *)&__m128i_result[0]) = 0x342caf9bffff1fff;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0xcc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xc6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000a16316b0;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000063636363;
  *((unsigned long *)&__m128i_op1[1]) = 0x16161616a16316b0;
  *((unsigned long *)&__m128i_op1[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000a16316b0;
  *((unsigned long *)&__m128i_result[0]) = 0x16161616a16316b0;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0xa7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff489b693120950;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc45a851c40c18;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffc45a851c40c18;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x48);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0xcc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000005d5d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x41);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffefefe6a;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000c2bac2c2;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000fefefe6a;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000c2bac2c2;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x7c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ffffffeffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x4080808080808080;
  *((unsigned long *)&__m128i_result[1]) = 0xff80ffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7ffffffeffffffff;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xe6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_op0[0]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000a0000000a;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000a00000009;
  *((unsigned long *)&__m128i_result[1]) = 0x000a000a0000000a;
  *((unsigned long *)&__m128i_result[0]) = 0x000a000a000a000a;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0xaf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff80000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x67);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x004fcfcfd01f9f9f;
  *((unsigned long *)&__m128i_op0[0]) = 0x9f4fcfcfcf800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x004fcfcfd01f9f9f;
  *((unsigned long *)&__m128i_op1[0]) = 0x9f4fcfcfcf800000;
  *((unsigned long *)&__m128i_result[1]) = 0x004f1fcfd01f9f9f;
  *((unsigned long *)&__m128i_result[0]) = 0x9f4fcfcfcf800000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xda);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x75b043c4d17db125;
  *((unsigned long *)&__m128i_op0[0]) = 0xeef8227b596117b1;
  *((unsigned long *)&__m128i_op1[1]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_op1[0]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_result[1]) = 0x75b043c4d17db125;
  *((unsigned long *)&__m128i_result[0]) = 0xeef8227b4f8017b1;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x027c027c000027c0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000de32400;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x027c027c000027c0;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x77);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363797c63996399;
  *((unsigned long *)&__m128i_op0[0]) = 0x171f0a1f6376441f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x6363797c63990099;
  *((unsigned long *)&__m128i_result[0]) = 0x171f0a1f6376441f;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0x94);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0bd80bd80bdfffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0bd80bd80bd80000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0bd80bd80bd80000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0xf9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x41dfbe1f41e0ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffc2ffe000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffc100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x41dfbe1f41e0ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffc100010001;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0xec);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xe93d0bd19ff0c170;
  *((unsigned long *)&__m128i_op1[0]) = 0x5237c1bac9eadf55;
  *((unsigned long *)&__m128i_result[1]) = 0x5237c1baffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x7d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffbd994889;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000a092444;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000890000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0x58);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000fea0000fffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff8607db959f;
  *((unsigned long *)&__m128i_op1[0]) = 0xff0cff78ff96ff14;
  *((unsigned long *)&__m128i_result[1]) = 0x00000fea0000fffe;
  *((unsigned long *)&__m128i_result[0]) = 0xff0cff78ff96ff14;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0xc2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x01ef013f01e701f8;
  *((unsigned long *)&__m128i_op1[0]) = 0x35bb8d32b2625c00;
  *((unsigned long *)&__m128i_result[1]) = 0x00008d3200000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0xea);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8003000000020000;
  *((unsigned long *)&__m128i_op0[0]) = 0x4040ffffc0400004;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8003000000020000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x64);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x74);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffff53d9;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff0001ffff9515;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffff53d9;
  *((unsigned long *)&__m128i_result[0]) = 0xff000001ffff9515;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0x67);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x80808080806b000b;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xf4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xc1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x71);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x82);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xd5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0xf3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xbbe5560400010001;
  *((unsigned long *)&__m128i_op0[0]) = 0xe7e5dabf00010001;
  *((unsigned long *)&__m128i_op1[1]) = 0xbbe5560400010001;
  *((unsigned long *)&__m128i_op1[0]) = 0xe7e5dabf00010001;
  *((unsigned long *)&__m128i_result[1]) = 0xe7e5560400010001;
  *((unsigned long *)&__m128i_result[0]) = 0xe7e5dabf00010001;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0xf3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x2c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x27);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x5d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x24);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000101010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xb6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x975ca6046e2e4889;
  *((unsigned long *)&__m128i_op1[0]) = 0x1748c4f9ed1a5870;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1748c4f9ed1a5870;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x6a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffc606ec5;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000014155445;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x76);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000024170000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000aa822a79308f6;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000024170000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x32);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000024170000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0x56);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vextrins_b (__m128i_op0, __m128i_op1, 0xc5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000034;
  *((unsigned long *)&__m128i_op1[1]) = 0x01017f3c00000148;
  *((unsigned long *)&__m128i_op1[0]) = 0x117d7f7b093d187f;
  *((unsigned long *)&__m128i_result[1]) = 0x117d7f7b093d187f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000034;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x70);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x01533b5e7489ae24;
  *((unsigned long *)&__m128i_op0[0]) = 0xe519ab7e71e33848;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x01533b5e7489ae24;
  *((unsigned long *)&__m128i_result[0]) = 0xffffab7e71e33848;
  __m128i_out = __lsx_vextrins_h (__m128i_op0, __m128i_op1, 0xbc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff760386bdae46;
  *((unsigned long *)&__m128i_op1[0]) = 0xc1fc7941bc7e00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffff7603;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0xc3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff2356fe165486;
  *((unsigned long *)&__m128i_op1[1]) = 0x3a8000003a800000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000ef0000000003b;
  *((unsigned long *)&__m128i_result[1]) = 0x0000003b0000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffff2356fe165486;
  __m128i_out = __lsx_vextrins_w (__m128i_op0, __m128i_op1, 0x70);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vextrins_d (__m128i_op0, __m128i_op1, 0x8a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
