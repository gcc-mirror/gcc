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
  *((unsigned long *)&__m128i_result[1]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0a0a0a0a0a0a0a;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[0]) = 0x1000100010001000;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, -10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000003be14000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000003bfb4000;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0b0b0b0b0b0b0b0b;
  *((unsigned long *)&__m128i_result[0]) = 0x0b0b0b0b0b0b0b0b;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007ffffffb;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_result[0]) = 0x010101017f010101;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000007f8;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000007f8;
  *((unsigned long *)&__m128i_result[1]) = 0x0b0b0b0b0b0b0b0b;
  *((unsigned long *)&__m128i_result[0]) = 0x0b0b0b0b0b0b0b0b;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000c;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000c;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0606060606060606;
  *((unsigned long *)&__m128i_result[0]) = 0x0606060606060606;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0fffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0fffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_b (__m128i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x027c027c000027c0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x027c027c000027c0;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002000000020000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000001fc00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000000020000;
  *((unsigned long *)&__m128i_result[0]) = 0x000001fc00000000;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000fff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000fff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000fff;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000007ff000ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000a1ff4c;
  *((unsigned long *)&__m128i_result[1]) = 0x000300037ff000ff;
  *((unsigned long *)&__m128i_result[0]) = 0x0003000300a10003;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[1]) = 0x000b000b000b000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000b000b000b000b;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_result[0]) = 0x0004000400040004;
  __m128i_out = __lsx_vmaxi_h (__m128i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfe07e5fefefdddfe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00020100fedd0c00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000201000000000b;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000401000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100000004;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, -10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000006f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000001f0a;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000006f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001f0a;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000050000007b;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000500000005;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000400000004;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001fffff001fffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x001fffff001fffff;
  *((unsigned long *)&__m128i_result[1]) = 0x001fffff001fffff;
  *((unsigned long *)&__m128i_result[0]) = 0x001fffff001fffff;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000b0000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000b0000000b;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000e0000000e;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000900000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000900000009;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x9c9c9c9c9c9c9c9c;
  *((unsigned long *)&__m128i_op0[0]) = 0x9c9c9c9c9c9c9c9c;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000600000006;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000600000006;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7f80000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x7f80000000000007;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000700000007;
  __m128i_out = __lsx_vmaxi_w (__m128i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000002;
  __m128i_out = __lsx_vmaxi_d (__m128i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000007f00;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000007f00;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001000000;
  __m128i_out = __lsx_vmaxi_d (__m128i_op0, -4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfff489b693120950;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffc45a851c40c18;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000a;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000a;
  __m128i_out = __lsx_vmaxi_d (__m128i_op0, 10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmaxi_d (__m128i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x63636b6afe486741;
  *((unsigned long *)&__m128i_op0[0]) = 0x41f8e880ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x63636b6afe486741;
  *((unsigned long *)&__m128i_result[0]) = 0x41f8e880ffffffff;
  __m128i_out = __lsx_vmaxi_d (__m128i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
