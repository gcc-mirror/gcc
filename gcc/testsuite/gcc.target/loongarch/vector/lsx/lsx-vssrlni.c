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
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff80000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001ffff00000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x2f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x4f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x004e005500060031;
  *((unsigned long *)&__m128i_op0[0]) = 0xff870068fff5ffb3;
  *((unsigned long *)&__m128i_op1[1]) = 0x004e005500060031;
  *((unsigned long *)&__m128i_op1[0]) = 0xff870068fff5ffb3;
  *((unsigned long *)&__m128i_result[1]) = 0x04e00060ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x04e00060ffffffff;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x52527d7d52527d7d;
  *((unsigned long *)&__m128i_op0[0]) = 0x52527d7d52527d7d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808000008080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080000080800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001010100010100;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x2f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000080007f80800;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000001000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00047fff00007fff;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ff0000ff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x01fc020000fe0100;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000003fc0003;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x56);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000017fda829;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x27);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0c03e17edd781b11;
  *((unsigned long *)&__m128i_op0[0]) = 0x342caf9bffff1fff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000040000000400;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0c037fff342c7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000fff8fff8;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fff80000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x37);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffff100fffc;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff00000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x21);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff100fffc;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffff100fffc;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000800080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x38);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffff800;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x001fffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x4b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_op1[0]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0a000a000a000a00;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf2f2e5e5e5e5e5dc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000003fc0;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x22);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x35);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffffffffffff;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x35);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0008000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[1]) = 0x41dfffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000083b00000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x33);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000003;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x7e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1ff85ffe2ae5d973;
  *((unsigned long *)&__m128i_op1[1]) = 0x403be000ffffe000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000000ffc2f;
  *((unsigned long *)&__m128i_result[0]) = 0x00201df000000000;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x29);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000005151515;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000006302e00;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000003f;
  *((unsigned long *)&__m128i_op1[0]) = 0x7f417f417f027e03;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001fd0;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x32);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001ffffff7f;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x5f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000202fe02;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000101;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x01203f1e3d1c3b1a;
  *((unsigned long *)&__m128i_op0[0]) = 0x3918371635143312;
  *((unsigned long *)&__m128i_op1[1]) = 0x21201f1e1d1c1b1a;
  *((unsigned long *)&__m128i_op1[0]) = 0x1918171615141312;
  *((unsigned long *)&__m128i_result[1]) = 0x480f7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff7fff7fff7fff;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00005dcbe7e830c0;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_op1[0]) = 0x1f5533a694f902c0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000001fffff59;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x63);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000007f41;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000002000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x39);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x14ccc6320076a4d2;
  *((unsigned long *)&__m128i_op0[0]) = 0x685670d27e00682a;
  *((unsigned long *)&__m128i_op1[1]) = 0x14ccc6320076a4d2;
  *((unsigned long *)&__m128i_op1[0]) = 0x685670d27e00682a;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc000000fc0003fff;
  *((unsigned long *)&__m128i_op0[0]) = 0xbffffff0ffffc00f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000003f0000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffc3ffff003e;
  *((unsigned long *)&__m128i_result[1]) = 0x00c0000000bfffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000ffffff;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x28);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x800000810000807f;
  *((unsigned long *)&__m128i_op0[0]) = 0x808080010080007f;
  *((unsigned long *)&__m128i_op1[1]) = 0x800000810000807f;
  *((unsigned long *)&__m128i_op1[0]) = 0x808080010080007f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000020000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000020000020;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x62);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0400400204004002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x6d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x2a29282726252423;
  *((unsigned long *)&__m128i_op1[0]) = 0x2221201f1e1d1c1b;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x26);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000002002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x2a29282726252423;
  *((unsigned long *)&__m128i_op1[0]) = 0x2221201f1e1d1c1b;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00a8009800880078;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000807f00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x80006b0080808080;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fff00007fff7fff;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000001010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000001fe01;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000001fe01;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_result[0]) = 0x0f0f0f0f00000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff010300ff0103;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x555500adfffc5cab;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010100000100;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x03ff0101fc010102;
  *((unsigned long *)&__m128i_op0[0]) = 0x03fffffffc010102;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff81010102;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff7fffffff;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x003f0000ffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x000300037ff000ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0003000300a10003;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_wu_d (__m128i_op0, __m128i_op1, 0x3c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000007070707;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x45);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffdfffcfffdfffc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffdfffcfffdfffc;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000053a4f452;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000053a;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_b_h (__m128i_op0, __m128i_op1, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000b3a6000067da;
  *((unsigned long *)&__m128i_op1[0]) = 0x00004e420000c26a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x7a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x67eb85afb2ebb000;
  *((unsigned long *)&__m128i_op1[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffffffffffff;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x38);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7c7c000000007176;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x3e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000c6c7;
  *((unsigned long *)&__m128i_op0[0]) = 0x8d8d8d8d8d8cc6c6;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_w_d (__m128i_op0, __m128i_op1, 0x3c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000aa822a8228222;
  *((unsigned long *)&__m128i_op0[0]) = 0x03aa558ec8546eb6;
  *((unsigned long *)&__m128i_op1[1]) = 0x001a64b345308091;
  *((unsigned long *)&__m128i_op1[0]) = 0x001f2f2cab1c732a;
  *((unsigned long *)&__m128i_result[1]) = 0x0155ffff754affff;
  *((unsigned long *)&__m128i_result[0]) = 0x034cffff03e5ffff;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc1bdceee242070dc;
  *((unsigned long *)&__m128i_op0[0]) = 0xe907b754d7eaa478;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_h_w (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_hu_w (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002711350a27112;
  *((unsigned long *)&__m128i_op0[0]) = 0x00d5701794027113;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_du_q (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000203000010d0;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffc00300000220;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x27);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000f50000000900;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000090900000998;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffffffffffff;
  __m128i_out = __lsx_vssrlni_d_q (__m128i_op0, __m128i_op1, 0x20);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001000010f8;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff8ffa2fffdffb0;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ff800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0f0f0f0f00000f00;
  __m128i_out = __lsx_vssrlni_bu_h (__m128i_op0, __m128i_op1, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
