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

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff8969ffffd7e2;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000d688ffffbd95;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xf12dfafc1ad1f7b3;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x34);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000200000002000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000200000002000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010000000100;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000001000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x2f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000c0002000c0002;
  *((unsigned long *)&__m128i_op0[0]) = 0x000400c600700153;
  *((unsigned long *)&__m128i_op1[1]) = 0x000c0002000c0002;
  *((unsigned long *)&__m128i_op1[0]) = 0x000400c600700153;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000010000007f;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000fffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0800000400000800;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000001515151500;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001515151500;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001515000015150;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fdfd0404;
  *((unsigned long *)&__m128i_op1[1]) = 0x3fffffff3fffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x3fffffff3fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000fc08;
  *((unsigned long *)&__m128i_result[0]) = 0x8000800080008000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000fc08;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000800080008000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffba420000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x000007e044000400;
  *((unsigned long *)&__m128i_result[0]) = 0xfdd2100000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000081e003f3f3f;
  *((unsigned long *)&__m128i_op0[0]) = 0x3f3f3f0e00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000081e003f3f3f;
  *((unsigned long *)&__m128i_op1[0]) = 0x3f3f3f0e00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000103c007e7e8;
  *((unsigned long *)&__m128i_result[0]) = 0x00000103c007e7e8;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x43);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0202022302023212;
  *((unsigned long *)&__m128i_op0[0]) = 0x0202ff3f02022212;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000002100003010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ff3f00002010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x79);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffff7fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xe2bb5ff00e20aceb;
  *((unsigned long *)&__m128i_op1[0]) = 0xe2bb5ff00e20aceb;
  *((unsigned long *)&__m128i_result[1]) = 0x0100010000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00e3000e00e3000e;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xf58df7841423142a;
  *((unsigned long *)&__m128i_op1[0]) = 0x3f7477f8ff4e2152;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x3d3e0505101e4008;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x2bd5d429e34a1efb;
  *((unsigned long *)&__m128i_op0[0]) = 0xfc0203fccbedbba7;
  *((unsigned long *)&__m128i_op1[1]) = 0xc9f66947f077afd0;
  *((unsigned long *)&__m128i_op1[0]) = 0x89fed7c07fdf5d00;
  *((unsigned long *)&__m128i_result[1]) = 0x14f1a50ffe65f6de;
  *((unsigned long *)&__m128i_result[0]) = 0xa3f83bd8e03fefaf;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x6ed694e00e0355db;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000010600000106;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xe00e035606000001;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xe739e7ade77ae725;
  *((unsigned long *)&__m128i_op0[0]) = 0xbb9013bd049bc9ec;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x56aca41400000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7ade77ae3bd049bd;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000041400000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x1010101010101010;
  *((unsigned long *)&__m128i_op1[0]) = 0x1010101010101010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8081808180818081;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000006ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0037f80000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x69);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0020202020202020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0080808080c04040;
  *((unsigned long *)&__m128i_op1[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[0]) = 0x0101010001808080;
  *((unsigned long *)&__m128i_result[1]) = 0x0000202000008081;
  *((unsigned long *)&__m128i_result[0]) = 0x0001010100010101;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x28);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0010000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00fff00000001000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x28);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x6b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000adf0000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001e00;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0040000000400040;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000020002020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808102;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000001010102;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001000100010000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x03fc03fc03fc03fc;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x04000400ff01ff01;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1010101010101010;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000fff800000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000001ed68;
  *((unsigned long *)&__m128i_op1[1]) = 0x1ff6a09e667f3bd8;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000007b5a;
  *((unsigned long *)&__m128i_result[0]) = 0x999fcef600000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffe5c8000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x91f80badc162a0c4;
  *((unsigned long *)&__m128i_op1[0]) = 0x99d1ffff0101ff01;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff400000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x905d0b06cf0008f8;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x3802f4fd025800f7;
  *((unsigned long *)&__m128i_op1[1]) = 0xc8ff0bffff00ffae;
  *((unsigned long *)&__m128i_op1[0]) = 0x91ff40fffff8ff50;
  *((unsigned long *)&__m128i_result[1]) = 0x0000200000000700;
  *((unsigned long *)&__m128i_result[0]) = 0x0000192000001240;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x33);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff0ffd0ffd;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff0ffc0001;
  *((unsigned long *)&__m128i_op1[1]) = 0xbb7743ca4c78461f;
  *((unsigned long *)&__m128i_op1[0]) = 0xd9743eb5fb4deb3a;
  *((unsigned long *)&__m128i_result[1]) = 0x003fffffffc3ff44;
  *((unsigned long *)&__m128i_result[0]) = 0x002eddd0f2931e12;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x4a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xbb7743ca4c78461f;
  *((unsigned long *)&__m128i_op0[0]) = 0xd9743eb5fb4deb3a;
  *((unsigned long *)&__m128i_op1[1]) = 0x22445e1ad9c3e4f0;
  *((unsigned long *)&__m128i_op1[0]) = 0x1b43e8a30a570a63;
  *((unsigned long *)&__m128i_result[1]) = 0x743ca4c843eb5fb5;
  *((unsigned long *)&__m128i_result[0]) = 0x45e1ad9c3e8a30a5;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x1204900f62f72565;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x4901725600000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x6a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000400000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000300000003;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x32);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x3f3f3f7fbf3fffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x47);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000040804080;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000020100000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffe8ffff28fc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_op1[1]) = 0x00007fff0000803e;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000006ffff81e1;
  *((unsigned long *)&__m128i_result[1]) = 0x0ffffffe8ffff290;
  *((unsigned long *)&__m128i_result[0]) = 0x000007fff0000804;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x44);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000418200000008e;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000002100047;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636362;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636362;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363636363636362;
  *((unsigned long *)&__m128i_op1[0]) = 0x6363636363636362;
  *((unsigned long *)&__m128i_result[1]) = 0x0032003200320032;
  *((unsigned long *)&__m128i_result[0]) = 0x0032003200320032;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff01010102;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ffdf87f0b0c7f7f;
  *((unsigned long *)&__m128i_op1[1]) = 0xf6b3eb63f6b3f6b3;
  *((unsigned long *)&__m128i_op1[0]) = 0x363953e42b56432e;
  *((unsigned long *)&__m128i_result[1]) = 0x010000010080000b;
  *((unsigned long *)&__m128i_result[0]) = 0x00f700f70036002b;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xed67d6c7ed67ed67;
  *((unsigned long *)&__m128i_op1[0]) = 0x6c72a7c856ac865c;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000700000003;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x3d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffff40ff83;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x1010101010101010;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000003030103;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000003030103;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000006060;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000006060;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000002408beb26c8;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000706e;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000028c27;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000070;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x80000b0b80000b0b;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000101080001010;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffefefffffeff0;
  *((unsigned long *)&__m128i_result[1]) = 0x0061006100020002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000fe00fe;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000078087f08;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000078087f08;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000e0fc0000e0fc;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ff0bff76;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x75);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x33);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff00ff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000ff00ffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x8282828282828282;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000828282828282;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0008000800000008;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00f7000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000005150;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000005150;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000f7000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x24);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x41afddcb1c000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xd09e1bd99a2c6eb1;
  *((unsigned long *)&__m128i_op1[0]) = 0xe82f7c27bb0778af;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000040002;
  *((unsigned long *)&__m128i_result[0]) = 0x000d000a000f000c;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffff8000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffdff0;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0144329880000000;
  *((unsigned long *)&__m128i_result[1]) = 0x007fffc0007ffff0;
  *((unsigned long *)&__m128i_result[0]) = 0x004000004c400000;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001e0000001e;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffafff0fff9ff01;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000d800cff8;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlrni_h_w (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000002000007d7;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000300000ff1;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x000007d700000ff1;
  __m128i_out = __lsx_vsrlrni_w_d (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff00ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffff00ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000ff8;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001000;
  __m128i_out = __lsx_vsrlrni_d_q (__m128i_op0, __m128i_op1, 0x74);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000f08;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x2020202020202020;
  __m128i_out = __lsx_vsrlrni_b_h (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
