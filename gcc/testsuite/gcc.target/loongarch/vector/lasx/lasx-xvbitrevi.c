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
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ff00ff00;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ff00ff00;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x01010101fe01fe01;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x01010101fe01fe01;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x2000200020002000;
  *((unsigned long *)&__m256i_result[2]) = 0x2000200020002000;
  *((unsigned long *)&__m256i_result[1]) = 0x2000200020002000;
  *((unsigned long *)&__m256i_result[0]) = 0x2000200020002000;
  __m256i_out = __lasx_xvbitrevi_h (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[2]) = 0x7fff7ff77fff7ff7;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[0]) = 0x7fff7ff77fff7ff7;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000020001;
  *((unsigned long *)&__m256i_result[3]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[2]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[1]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[0]) = 0x1010101010121011;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000004000000040;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020202020;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000020000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000020000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000020000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000020000000000;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x29);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0001c4e8ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0001c4e8ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0080000000800000;
  *((unsigned long *)&__m256i_result[2]) = 0x0081c4e8ff7fffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0080000000800000;
  *((unsigned long *)&__m256i_result[0]) = 0x0081c4e8ff7fffff;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffff81ff7d;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffff81ff7d;
  *((unsigned long *)&__m256i_result[3]) = 0x7f7f7f7f7f7f7f7f;
  *((unsigned long *)&__m256i_result[2]) = 0x7f7f7f7f7f017ffd;
  *((unsigned long *)&__m256i_result[1]) = 0x7f7f7f7f7f7f7f7f;
  *((unsigned long *)&__m256i_result[0]) = 0x7f7f7f7f7f017ffd;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x4000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x4000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x4000000000000000;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x3e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000002080100;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000002080100;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000008000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000a080100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000008000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000a080100;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0100010001000100;
  __m256i_out = __lasx_xvbitrevi_h (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffe0047d00e00480;
  *((unsigned long *)&__m256i_op0[2]) = 0x001fc0200060047a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffe0047d00e00480;
  *((unsigned long *)&__m256i_op0[0]) = 0x001fc0200060047a;
  *((unsigned long *)&__m256i_result[3]) = 0xfee1057c01e10581;
  *((unsigned long *)&__m256i_result[2]) = 0x011ec1210161057b;
  *((unsigned long *)&__m256i_result[1]) = 0xfee1057c01e10581;
  *((unsigned long *)&__m256i_result[0]) = 0x011ec1210161057b;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_result[3]) = 0xfdfdfdfdfdfdfdfd;
  *((unsigned long *)&__m256i_result[2]) = 0xe27fe2821d226278;
  *((unsigned long *)&__m256i_result[1]) = 0xfdfdfdfdfdfdfdfd;
  *((unsigned long *)&__m256i_result[0]) = 0xe27fe2821d226278;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000200000002;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000800000008;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000800200028;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000800200027;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000800200028;
  *((unsigned long *)&__m256i_result[3]) = 0x080808000828082f;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080008280820;
  *((unsigned long *)&__m256i_result[1]) = 0x080808000828082f;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080008280820;
  __m256i_out = __lasx_xvbitrevi_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvbitrevi_h (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000800000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000800000000000;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x2f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0200000002000000;
  *((unsigned long *)&__m256i_result[2]) = 0x02000000fdffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0200000002000000;
  *((unsigned long *)&__m256i_result[0]) = 0x02000000fdffffff;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffeffed;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffeffed;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffeffed;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffeffed;
  __m256i_out = __lasx_xvbitrevi_d (__m256i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc039000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xc039000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xc039000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xc039000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xc03b000200020002;
  *((unsigned long *)&__m256i_result[2]) = 0xc03b000200020002;
  *((unsigned long *)&__m256i_result[1]) = 0xc03b000200020002;
  *((unsigned long *)&__m256i_result[0]) = 0xc03b000200020002;
  __m256i_out = __lasx_xvbitrevi_h (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff80007fff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff80007fff0000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000010000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x7fff81007fff0100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000010000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x7fff81007fff0100;
  __m256i_out = __lasx_xvbitrevi_w (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
