/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lasxintrin.h>

int
main ()
{
  __m256i __m256i_op0, __m256i_op1, __m256i_op2, __m256i_out, __m256i_result;
  __m256 __m256_op0, __m256_op1, __m256_op2, __m256_out, __m256_result;
  __m256d __m256d_op0, __m256d_op1, __m256d_op2, __m256d_out, __m256d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffe06df8d7;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffbe8b470f;
  *((unsigned long *)&__m256i_result[3]) = 0x7ffffffffffff7ff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffe06df0d7;
  *((unsigned long *)&__m256i_result[1]) = 0x7ffffffffffff7ff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffbe8b470f;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0010ffc80010ff52;
  *((unsigned long *)&__m256i_op0[2]) = 0xfff1ffca0011ffcb;
  *((unsigned long *)&__m256i_op0[1]) = 0x0010ffc80010ff52;
  *((unsigned long *)&__m256i_op0[0]) = 0xfff1ffca0011ffcb;
  *((unsigned long *)&__m256i_result[3]) = 0x0010bfc80010bf52;
  *((unsigned long *)&__m256i_result[2]) = 0xfff1bfca0011bfcb;
  *((unsigned long *)&__m256i_result[1]) = 0x0010bfc80010bf52;
  *((unsigned long *)&__m256i_result[0]) = 0xfff1bfca0011bfcb;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000005536aaaaac;
  *((unsigned long *)&__m256i_op0[2]) = 0x55555555aaaaaaac;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000005536aaaaac;
  *((unsigned long *)&__m256i_op0[0]) = 0x55555555aaaaaaac;
  *((unsigned long *)&__m256i_result[3]) = 0x0000005136aaaaa8;
  *((unsigned long *)&__m256i_result[2]) = 0x55515551aaaaaaa8;
  *((unsigned long *)&__m256i_result[1]) = 0x0000005136aaaaa8;
  *((unsigned long *)&__m256i_result[0]) = 0x55515551aaaaaaa8;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fff7fff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x7fdf000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fdf000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x7fdf7fff00000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fdf7fff00000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x35);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000fd0000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000fd0000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7ffe7ffe7ffe7ffe;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000007ffe7ffe;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ffe7ffe7ffe8000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000807e7ffe;
  *((unsigned long *)&__m256i_result[3]) = 0x7f7e7f7e7f7e7f7e;
  *((unsigned long *)&__m256i_result[2]) = 0x000000007f7e7f7e;
  *((unsigned long *)&__m256i_result[1]) = 0x7f7e7f7e7f7e0000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000007e7f7e;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x24);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xdf01010153a10101;
  *((unsigned long *)&__m256i_op0[2]) = 0x5b7f01ff5b7f10ff;
  *((unsigned long *)&__m256i_op0[1]) = 0xdf01010153a10101;
  *((unsigned long *)&__m256i_op0[0]) = 0x5b7f01ff5b7f10ff;
  *((unsigned long *)&__m256i_result[3]) = 0xcf01010143a10101;
  *((unsigned long *)&__m256i_result[2]) = 0x4b6f01ef4b6f00ef;
  *((unsigned long *)&__m256i_result[1]) = 0xcf01010143a10101;
  *((unsigned long *)&__m256i_result[0]) = 0x4b6f01ef4b6f00ef;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[3]) = 0xdfffffffdfffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[1]) = 0xdfffffffdfffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x8000000080000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffff02ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffff0100;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00fe00feff02ff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ff7fff7f;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ff7f027f;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ff7f0100;
  *((unsigned long *)&__m256i_result[0]) = 0xff00fe00fe7f027f;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8011ffee804c004c;
  *((unsigned long *)&__m256i_op0[2]) = 0x00faff0500c3ff3c;
  *((unsigned long *)&__m256i_op0[1]) = 0x80f900f980780078;
  *((unsigned long *)&__m256i_op0[0]) = 0x0057ffa800ceff31;
  *((unsigned long *)&__m256i_result[3]) = 0x8011ffae800c000c;
  *((unsigned long *)&__m256i_result[2]) = 0x00baff050083ff3c;
  *((unsigned long *)&__m256i_result[1]) = 0x80b900b980380038;
  *((unsigned long *)&__m256i_result[0]) = 0x0017ffa8008eff31;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x3b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000027262524;
  *((unsigned long *)&__m256i_op0[2]) = 0x232221201f1e1d1c;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000027262524;
  *((unsigned long *)&__m256i_op0[0]) = 0x232221201f1e1d1c;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000027262524;
  *((unsigned long *)&__m256i_result[2]) = 0x23222120171e151c;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000027262524;
  *((unsigned long *)&__m256i_result[0]) = 0x23222120171e151c;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00ff00fe00ff00fe;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00ff00fe00ff00fe;
  *((unsigned long *)&__m256i_result[3]) = 0x0000fefe0000fefe;
  *((unsigned long *)&__m256i_result[2]) = 0x00fe00fe00fe00fe;
  *((unsigned long *)&__m256i_result[1]) = 0x0000fefe0000fefe;
  *((unsigned long *)&__m256i_result[0]) = 0x00fe00fe00fe00fe;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000009;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x26);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_op0[2]) = 0xffe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_op0[1]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_op0[0]) = 0xffe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_result[3]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_result[2]) = 0x7fe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_result[1]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_result[0]) = 0x7fe1ffe0ffe1ffe0;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x3f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_d (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffbfffffffb;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000fffffffb;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffbfffffffb;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000fffffffb;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000800200028;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000800200028;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000800200028;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000800200028;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffee00ba;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffee00ba;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xefefefefefee00aa;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xefefefefefee00aa;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_op0[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_op0[0]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100010;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ff88ff88;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ff88ff88;
  *((unsigned long *)&__m256i_result[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000f788f788;
  *((unsigned long *)&__m256i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000f788f788;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitclri_w (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_result[2]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_result[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_result[0]) = 0xfffefffefffefffe;
  __m256i_out = __lasx_xvbitclri_h (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
