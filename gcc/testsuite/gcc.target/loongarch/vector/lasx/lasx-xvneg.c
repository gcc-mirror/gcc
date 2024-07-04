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

  *((unsigned long *)&__m256i_op0[3]) = 0x002e4db200000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000315ac0000d658;
  *((unsigned long *)&__m256i_op0[1]) = 0x00735278007cf94c;
  *((unsigned long *)&__m256i_op0[0]) = 0x0003ed8800031b38;
  *((unsigned long *)&__m256i_result[3]) = 0xffd1b24e00000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfffcea54ffff29a8;
  *((unsigned long *)&__m256i_result[1]) = 0xff8cad88ff8306b4;
  *((unsigned long *)&__m256i_result[0]) = 0xfffc1278fffce4c8;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000ffff8000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x06f880008000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[0]) = 0x800080008000b8f1;
  *((unsigned long *)&__m256i_result[3]) = 0x8000010180000101;
  *((unsigned long *)&__m256i_result[2]) = 0xfa08800080000101;
  *((unsigned long *)&__m256i_result[1]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_result[0]) = 0x800080008000480f;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffefffffefc;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010102;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010201010204;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010102;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010102;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffffffefd;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010203;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffff8c80;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000fff0e400;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000007380;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000f1c00;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000100000001;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000800000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000100000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x007f00ff007f00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x007f00ff007f00ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x007f00ff007f00ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x007f00ff007f00ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0081000100810001;
  *((unsigned long *)&__m256i_result[2]) = 0x0081000100810001;
  *((unsigned long *)&__m256i_result[1]) = 0x0081000100810001;
  *((unsigned long *)&__m256i_result[0]) = 0x0081000100810001;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x223d76f09f3881ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x3870ca8d013e76a0;
  *((unsigned long *)&__m256i_op0[1]) = 0x223d76f09f37e357;
  *((unsigned long *)&__m256i_op0[0]) = 0x43ec0a1b2aba7ed0;
  *((unsigned long *)&__m256i_result[3]) = 0xdec38a1061c87f01;
  *((unsigned long *)&__m256i_result[2]) = 0xc8903673ffc28a60;
  *((unsigned long *)&__m256i_result[1]) = 0xdec38a1061c91da9;
  *((unsigned long *)&__m256i_result[0]) = 0xbd14f6e5d6468230;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000007e8080;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000001fdda7dc4;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000007e8080;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000001fdda7dc4;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ff827f80;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ffff0226823c;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ff827f80;
  *((unsigned long *)&__m256i_result[0]) = 0x0000ffff0226823c;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000180000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x8000000180000001;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffff00000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffff00000001;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000f000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000f000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfff1000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfff1000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff8000ffa3;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000008000165a;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff8000ffa3;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000008000165a;
  *((unsigned long *)&__m256i_result[3]) = 0xffff00017fff005d;
  *((unsigned long *)&__m256i_result[2]) = 0x000000007fffe9a6;
  *((unsigned long *)&__m256i_result[1]) = 0xffff00017fff005d;
  *((unsigned long *)&__m256i_result[0]) = 0x000000007fffe9a6;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf7f7f7f7f7f7f7f8;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xf7f7f7f7f7f7f7f8;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000ffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000ffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffff0100000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffff0100000001;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op0[2]) = 0x0100004300000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op0[0]) = 0x0100004300000000;
  *((unsigned long *)&__m256i_result[3]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_result[2]) = 0xff0000bd00000000;
  *((unsigned long *)&__m256i_result[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_result[0]) = 0xff0000bd00000000;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000080040;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000010000080040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000080040;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000010000080040;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000fff8ffc0;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ff00fff8ffc0;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000fff8ffc0;
  *((unsigned long *)&__m256i_result[0]) = 0x0000ff00fff8ffc0;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000010001;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000497fe0000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000683fe0000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000497fe0000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000683fe0000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffb6811fffff80;
  *((unsigned long *)&__m256i_result[2]) = 0xffff97c120000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffb6811fffff80;
  *((unsigned long *)&__m256i_result[0]) = 0xffff97c120000000;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfefefefefdfdfdfd;
  *((unsigned long *)&__m256i_op0[2]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_op0[1]) = 0xfefefefefdfdfdfd;
  *((unsigned long *)&__m256i_op0[0]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010202020203;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010201010102;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010202020203;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010201010102;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000032;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000032;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffffffce;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffffffce;
  __m256i_out = __lasx_xvneg_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvneg_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00007fde00007fd4;
  *((unsigned long *)&__m256i_op0[2]) = 0x00007fe000007fe0;
  *((unsigned long *)&__m256i_op0[1]) = 0x00007fde00007fd4;
  *((unsigned long *)&__m256i_op0[0]) = 0x00007fe000007fe0;
  *((unsigned long *)&__m256i_result[3]) = 0x000081220000812c;
  *((unsigned long *)&__m256i_result[2]) = 0x0000812000008120;
  *((unsigned long *)&__m256i_result[1]) = 0x000081220000812c;
  *((unsigned long *)&__m256i_result[0]) = 0x0000812000008120;
  __m256i_out = __lasx_xvneg_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000002780;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000002780;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffd880;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffd880;
  __m256i_out = __lasx_xvneg_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
