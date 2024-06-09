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

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000800;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000800;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffcf800fffcf800;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000080000000800;
  *((unsigned long *)&__m256i_result[2]) = 0xfffcf800fffcf800;
  *((unsigned long *)&__m256i_result[1]) = 0x0000080000000800;
  *((unsigned long *)&__m256i_result[0]) = 0x0000080000000800;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00007f7f00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00007f7f00007fff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000040000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00007f7f00000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000040000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00007f7f00007fff;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x2a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[2]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[1]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[0]) = 0x0202020202020202;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000800000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000800000000;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x23);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[2]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[1]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[0]) = 0x1010101010101010;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000004000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000004000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000004000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000004000000;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000013;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000001000000fe;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000013;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000001000000fb;
  *((unsigned long *)&__m256i_result[3]) = 0x8080808180808093;
  *((unsigned long *)&__m256i_result[2]) = 0x80808081808080fe;
  *((unsigned long *)&__m256i_result[1]) = 0x8080808180808093;
  *((unsigned long *)&__m256i_result[0]) = 0x80808081808080fb;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000020;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0010000000100000;
  *((unsigned long *)&__m256i_result[2]) = 0x0010000000100000;
  *((unsigned long *)&__m256i_result[1]) = 0x0010000000100000;
  *((unsigned long *)&__m256i_result[0]) = 0x0010000000100000;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100010;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000044444443;
  *((unsigned long *)&__m256i_op0[2]) = 0x7bbbbbbbf7777778;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000044444443;
  *((unsigned long *)&__m256i_op0[0]) = 0x7bbbbbbbf7777778;
  *((unsigned long *)&__m256i_result[3]) = 0x1000100054445443;
  *((unsigned long *)&__m256i_result[2]) = 0x7bbbbbbbf7777778;
  *((unsigned long *)&__m256i_result[1]) = 0x1000100054445443;
  *((unsigned long *)&__m256i_result[0]) = 0x7bbbbbbbf7777778;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020202020;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000200;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffa0078fffa0074;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffa0078fffa0074;
  *((unsigned long *)&__m256i_result[3]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_result[2]) = 0xfffa2078fffa2074;
  *((unsigned long *)&__m256i_result[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m256i_result[0]) = 0xfffa2078fffa2074;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffeffebfb7afb62;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffeffebfb7afb62;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000800;
  *((unsigned long *)&__m256i_result[2]) = 0xfffeffebfb7afb62;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000800;
  *((unsigned long *)&__m256i_result[0]) = 0xfffeffebfb7afb62;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_op0[2]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_op0[1]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_op0[0]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[3]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[2]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[1]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[0]) = 0xe7e7e7e7e7e7e7e7;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000004411;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000004411;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020206431;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020206431;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0003030300000300;
  *((unsigned long *)&__m256i_op0[2]) = 0x0003030300000300;
  *((unsigned long *)&__m256i_op0[1]) = 0x0003030300000100;
  *((unsigned long *)&__m256i_op0[0]) = 0x0003030300000100;
  *((unsigned long *)&__m256i_result[3]) = 0x0043030300400300;
  *((unsigned long *)&__m256i_result[2]) = 0x0043030300400300;
  *((unsigned long *)&__m256i_result[1]) = 0x0043030300400100;
  *((unsigned long *)&__m256i_result[0]) = 0x0043030300400100;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x223d76f09f3881ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x3870ca8d013e76a0;
  *((unsigned long *)&__m256i_op0[1]) = 0x223d76f09f37e357;
  *((unsigned long *)&__m256i_op0[0]) = 0x43ec0a1b2aba7ed0;
  *((unsigned long *)&__m256i_result[3]) = 0x223d76f09f3881ff;
  *((unsigned long *)&__m256i_result[2]) = 0x3870ca9d013e76b0;
  *((unsigned long *)&__m256i_result[1]) = 0x223d76f09f37e357;
  *((unsigned long *)&__m256i_result[0]) = 0x43ec0a1b2aba7ed0;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xf7f8f7f8f800f800;
  *((unsigned long *)&__m256i_op0[2]) = 0x00003f780000ff80;
  *((unsigned long *)&__m256i_op0[1]) = 0xf7f8f7f80000fff9;
  *((unsigned long *)&__m256i_op0[0]) = 0x00003f780000ff80;
  *((unsigned long *)&__m256i_result[3]) = 0xf7f8f7f8f800f800;
  *((unsigned long *)&__m256i_result[2]) = 0x00003f784000ff80;
  *((unsigned long *)&__m256i_result[1]) = 0xf7f8f7f84000fff9;
  *((unsigned long *)&__m256i_result[0]) = 0x00003f784000ff80;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff0020ff1f001f;
  *((unsigned long *)&__m256i_op0[2]) = 0xffe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff0020ff1f001f;
  *((unsigned long *)&__m256i_op0[0]) = 0xffe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_result[3]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_result[2]) = 0xffe1ffe0ffe1ffe0;
  *((unsigned long *)&__m256i_result[1]) = 0x01ff0020ff1f001f;
  *((unsigned long *)&__m256i_result[0]) = 0xffe1ffe0ffe1ffe0;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffefef800;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffefef800;
  *((unsigned long *)&__m256i_result[3]) = 0x0000008000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffefef800;
  *((unsigned long *)&__m256i_result[1]) = 0x0000008000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffefef800;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x27);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0002000000020000;
  *((unsigned long *)&__m256i_result[2]) = 0x0002000000020000;
  *((unsigned long *)&__m256i_result[1]) = 0x0002000000020000;
  *((unsigned long *)&__m256i_result[0]) = 0x0002000000020000;
  __m256i_out = __lasx_xvbitseti_w (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000030b8;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000030b8;
  *((unsigned long *)&__m256i_result[3]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[2]) = 0x00020002000230ba;
  *((unsigned long *)&__m256i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[0]) = 0x00020002000230ba;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_result[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[2]) = 0x8100810081008100;
  *((unsigned long *)&__m256i_result[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[0]) = 0x8100810081008100;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000007878;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000007878;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000107878;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000107878;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffb2f600006f48;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffb2f600006f48;
  *((unsigned long *)&__m256i_result[3]) = 0x4000400140004001;
  *((unsigned long *)&__m256i_result[2]) = 0xfffff2f640006f48;
  *((unsigned long *)&__m256i_result[1]) = 0x4000400140004001;
  *((unsigned long *)&__m256i_result[0]) = 0xfffff2f640006f48;
  __m256i_out = __lasx_xvbitseti_h (__m256i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000008;
  __m256i_out = __lasx_xvbitseti_d (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfd02fd02fd02fd02;
  *((unsigned long *)&__m256i_op0[2]) = 0xfd02fd02fd02fd02;
  *((unsigned long *)&__m256i_op0[1]) = 0xfd02fd02fd02fd02;
  *((unsigned long *)&__m256i_op0[0]) = 0xfd02fd02fd02fd02;
  *((unsigned long *)&__m256i_result[3]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_result[2]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_result[1]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_result[0]) = 0xfd12fd12fd12fd12;
  __m256i_out = __lasx_xvbitseti_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
