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
  *((unsigned long *)&__m128i_op1[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1e801ffc7fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00003fe00ffe3fe0;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff00ff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000001f;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x7b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc39fffff007fffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000fe00fd;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x78c00000ff000000;
  *((unsigned long *)&__m128i_result[1]) = 0x61cf003f0000007f;
  *((unsigned long *)&__m128i_result[0]) = 0x000000003c607f80;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff7f01ff01;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff7f01ff01;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffe03;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffe03;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff8001ffff8001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001ffff0003ffff0;
  *((unsigned long *)&__m128i_result[0]) = 0x000fffefffefffef;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x4b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363797c63990099;
  *((unsigned long *)&__m128i_op0[0]) = 0x171f0a1f6376441f;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363797c63990099;
  *((unsigned long *)&__m128i_op1[0]) = 0x171f0a1f6376441f;
  *((unsigned long *)&__m128i_result[1]) = 0x181e180005021811;
  *((unsigned long *)&__m128i_result[0]) = 0x181e180005021811;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00003fff00003fff;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf0fd800080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000a00028004000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000f000800000002;
  *((unsigned long *)&__m128i_result[0]) = 0x000f000000000000;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xaeaeaeaeaeaeae35;
  *((unsigned long *)&__m128i_op0[0]) = 0xaeaeaeaeaeaeae35;
  *((unsigned long *)&__m128i_op1[1]) = 0xaeaeaeaeaeaeae35;
  *((unsigned long *)&__m128i_op1[0]) = 0xaeaeaeaeaeaeae35;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000200000002;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x3e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff00ff00000000;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000008140c80;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000008140c80;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000002050320;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000002050320;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000002050320;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000002050320;
  *((unsigned long *)&__m128i_op1[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[0]) = 0x010101017f010101;
  *((unsigned long *)&__m128i_result[1]) = 0x0000040600000406;
  *((unsigned long *)&__m128i_result[0]) = 0x020202020202fe02;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_op1[0]) = 0xe364525335ede000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000fff00000e36;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x34);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x601fbfbeffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfff8000000000000;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsrlni_h_w (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000455555555;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000008;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_w_d (__m128i_op0, __m128i_op1, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7c7c000000007176;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000f3040705;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000001f1f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x32);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000000bffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000040001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x6d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xe4c8b96e2560afe9;
  *((unsigned long *)&__m128i_op0[0]) = 0xc001a1867fffa207;
  *((unsigned long *)&__m128i_op1[1]) = 0xe4c8b96e2560afe9;
  *((unsigned long *)&__m128i_op1[0]) = 0xc001a1867fffa207;
  *((unsigned long *)&__m128i_result[1]) = 0xe2560afe9c001a18;
  *((unsigned long *)&__m128i_result[0]) = 0xe2560afe9c001a18;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x24);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000042ab41;
  *((unsigned long *)&__m128i_op0[0]) = 0xb1b1b1b1b16f0670;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000044470000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000202020200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000100;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x020310edc003023d;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000080c43b700;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x56);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x30eb022002101b20;
  *((unsigned long *)&__m128i_op0[0]) = 0x020310edc003023d;
  *((unsigned long *)&__m128i_op1[1]) = 0x30eb022002101b20;
  *((unsigned long *)&__m128i_op1[0]) = 0x020310edc003023d;
  *((unsigned long *)&__m128i_result[1]) = 0x022002101b200203;
  *((unsigned long *)&__m128i_result[0]) = 0x022002101b200203;
  __m128i_out = __lsx_vsrlni_d_q (__m128i_op0, __m128i_op1, 0x30);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrlni_b_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
