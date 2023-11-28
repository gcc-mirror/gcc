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
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000003;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000030000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0xc9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0004007c00fc0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x047c0404fc00fcfc;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x8a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x007fffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff00ff7f00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x32);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x85);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff51cf8da;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffd6040188;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff01018888;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x50);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x007d00c50177ac5b;
  *((unsigned long *)&__m128i_op0[0]) = 0xac82aa88a972a36a;
  *((unsigned long *)&__m128i_result[1]) = 0x000000c5ac01015b;
  *((unsigned long *)&__m128i_result[0]) = 0xaaacac88a3a9a96a;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x7c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000a0000000a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000a00000009;
  *((unsigned long *)&__m128i_result[1]) = 0x0a0a0a000a0a0a00;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0a0a0009090900;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000001000100;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001000100;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00003f8000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00003f8000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x003f800000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x003f800000000000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0xd2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x6c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x81);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000dffff000d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000ffffff;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x6b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5f675e96e29a5a60;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[1]) = 0x965f5e9660e25a60;
  *((unsigned long *)&__m128i_result[0]) = 0xff7f7fffff7f7fff;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x34);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x131211101211100f;
  *((unsigned long *)&__m128i_op0[0]) = 0x11100f0e100f0e0d;
  *((unsigned long *)&__m128i_result[1]) = 0x13101213120f1112;
  *((unsigned long *)&__m128i_result[0]) = 0x110e1011100d0f10;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0xcb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001000110;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000431f851f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000001011010;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000043431f1f;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0xf0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128i_op0[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128i_result[1]) = 0xd1c0c0a5baf8f8d3;
  *((unsigned long *)&__m128i_result[0]) = 0xecbbbbc5d5f3f3f3;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0x7c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000454ffff9573;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_result[0]) = 0x00000454ffff9573;
  __m128i_out = __lsx_vshuf4i_b (__m128i_op0, 0xa4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0xf3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0x2c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0xd2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x003f000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x007c000d00400000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000003f00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000007c00000040;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0x31);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff00000000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0xb9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ffffffe00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff00007fff0000;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0xcd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff00000000ffff;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0x93);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000007f7f7f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x007f007f00007f7f;
  __m128i_out = __lsx_vshuf4i_h (__m128i_op0, 0x58);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000080808000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000080808000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x8b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffdfffdfffdfffd;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffdfffdfffdfffd;
  *((unsigned long *)&__m128i_result[1]) = 0xfffdfffdfffdfffd;
  *((unsigned long *)&__m128i_result[0]) = 0xfffdfffdfffdfffd;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x7e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfefefefdbffefdfe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfefefeeffef7fefe;
  *((unsigned long *)&__m128i_result[1]) = 0xfef7fefebffefdfe;
  *((unsigned long *)&__m128i_result[0]) = 0xfefefefdfefefeef;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x2d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x002a001a001a000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000002a001a;
  *((unsigned long *)&__m128i_result[0]) = 0x001a000b00000000;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x78);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x98);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000010f8000081a2;
  *((unsigned long *)&__m128i_op0[0]) = 0x000069bb00000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001000010f8;
  __m128i_out = __lsx_vshuf4i_w (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x44);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fffff800;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000fffff800;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000fffff800;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x8a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000006f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000001f0a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000006f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x36);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffda6e;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffe3d6;
  *((unsigned long *)&__m128i_op1[1]) = 0xeeb1e4f4bc3763f3;
  *((unsigned long *)&__m128i_op1[0]) = 0x6f5edf5ada6fe3d7;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffe3d6;
  *((unsigned long *)&__m128i_result[0]) = 0xeeb1e4f4bc3763f3;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x23);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100200001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100200001;
  *((unsigned long *)&__m128i_op1[1]) = 0x00001fff00001fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xce23d33e43d9736c;
  *((unsigned long *)&__m128i_op1[0]) = 0x63b2ac27aa076aeb;
  *((unsigned long *)&__m128i_result[1]) = 0x63b2ac27aa076aeb;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0xc8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000158;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000158;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0xc9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0xbf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x801d5de0000559e0;
  *((unsigned long *)&__m128i_op0[0]) = 0x77eb86788eebaf00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x2e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x89582bf870006860;
  *((unsigned long *)&__m128i_op1[0]) = 0x89582bf870006860;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vshuf4i_d (__m128i_op0, __m128i_op1, 0x94);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
