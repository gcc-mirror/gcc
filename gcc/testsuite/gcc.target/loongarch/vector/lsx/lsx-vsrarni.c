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
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ff020000fff4;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ff020000fff4;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fc0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1e801ffc00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000080007f80800;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ff0000ff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x4b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000001e5;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x5000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff8000002f4ef4a8;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000f4a8;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00100184017e0032;
  *((unsigned long *)&__m128i_op0[0]) = 0x0086018c01360164;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffff33c4b1e67;
  *((unsigned long *)&__m128i_result[1]) = 0x0000800c0004300c;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x25);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x66);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4101010141010100;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000000001ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0020808100000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x29);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x64);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x001ffff0003ffff0;
  *((unsigned long *)&__m128i_op1[0]) = 0x028c026bfff027af;
  *((unsigned long *)&__m128i_result[1]) = 0x00000003fc03fc00;
  *((unsigned long *)&__m128i_result[0]) = 0xffffc00a3009b000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ffa7f8ff81;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000003f0080ffc0;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000007fff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000a7f87fffff81;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffd400000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000004000000040;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x003f800000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x003f800000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000080003f80ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000000020000;
  *((unsigned long *)&__m128i_result[0]) = 0x000001fc00000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff80010001;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff80010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0bd80bd80bdfffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0bd80bd80bd80000;
  *((unsigned long *)&__m128i_result[1]) = 0x1ffffffff8001000;
  *((unsigned long *)&__m128i_result[0]) = 0xf0bd80bd80bd8000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x24);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xecec006c00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xecec006c00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff007f00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff007f00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000001ff85ffdc0;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000332ae5d97330;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x1ff85ffe2ae5d973;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000043c5ea7b6;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000008fc4ef7b4;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000fea0000fffe;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x48);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000dfa6e0c6;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000d46cdc13;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x64);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x09e8e9012fded7fd;
  *((unsigned long *)&__m128i_op0[0]) = 0x479f64b03373df61;
  *((unsigned long *)&__m128i_op1[1]) = 0x04c0044a0400043a;
  *((unsigned long *)&__m128i_op1[0]) = 0x04c004d6040004c6;
  *((unsigned long *)&__m128i_result[1]) = 0x1d20db00ec967bec;
  *((unsigned long *)&__m128i_result[0]) = 0x00890087009b0099;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000080800000808;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000080800000808;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8080000180800001;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000003e;
  *((unsigned long *)&__m128i_op1[1]) = 0x00fe00fe000200fe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00fe00fe000200fe;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000003e;
  *((unsigned long *)&__m128i_result[0]) = 0xfefe02fefefe02fe;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000200000002000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1000000010000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000020000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0103000201030002;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x3f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x26);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffc000400000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00003fff00010000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x6d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ff010000ff01;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xcd636363cd636363;
  *((unsigned long *)&__m128i_op0[0]) = 0xcd636363cd636363;
  *((unsigned long *)&__m128i_op1[1]) = 0xcd636363cd636363;
  *((unsigned long *)&__m128i_op1[0]) = 0xcd636363cd636363;
  *((unsigned long *)&__m128i_result[1]) = 0xf359f359f359f359;
  *((unsigned long *)&__m128i_result[0]) = 0xf359f359f359f359;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000016;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000016;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_b_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x01533b5e7489ae24;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffab7e71e33848;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xce9135c49ffff570;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_d_q (__m128i_op0, __m128i_op1, 0x23);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000807bf0a1f80;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000800ecedee68;
  *((unsigned long *)&__m128i_op1[1]) = 0x0005840100000005;
  *((unsigned long *)&__m128i_op1[0]) = 0x0005847b00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001f0a20001cedf;
  *((unsigned long *)&__m128i_result[0]) = 0x0058000000580000;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffb1fb1000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xf2c97aaa7d8fa270;
  *((unsigned long *)&__m128i_op1[0]) = 0x0b73e427f7cfcb88;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsrarni_w_d (__m128i_op0, __m128i_op1, 0x3f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0a545374471b7070;
  *((unsigned long *)&__m128i_op0[0]) = 0x274f4f0648145f50;
  *((unsigned long *)&__m128i_op1[1]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_op1[0]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_result[1]) = 0xa8a736e19e9e28bf;
  *((unsigned long *)&__m128i_result[0]) = 0x9e9f9e9f9e9f9e9f;
  __m128i_out = __lsx_vsrarni_h_w (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
