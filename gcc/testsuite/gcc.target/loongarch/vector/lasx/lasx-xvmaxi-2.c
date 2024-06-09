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
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000102;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m256i_result[2]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m256i_result[1]) = 0x0a0a0a0a0a0a0a0a;
  *((unsigned long *)&__m256i_result[0]) = 0x0a0a0a0a0a0a0a0a;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1717171717171717;
  *((unsigned long *)&__m256i_result[2]) = 0x1717171717171717;
  *((unsigned long *)&__m256i_result[1]) = 0x1717171717171717;
  *((unsigned long *)&__m256i_result[0]) = 0x1717171717171717;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ffe00007f000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x1616161616161616;
  *((unsigned long *)&__m256i_result[2]) = 0x161616167fffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x7ffe16167f161616;
  *((unsigned long *)&__m256i_result[0]) = 0x161616167fffffff;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000feb60000b7d0;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000feb60000c7eb;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000feb60000b7d0;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000feb60000c7eb;
  *((unsigned long *)&__m256i_result[3]) = 0x0707feb60707b7d0;
  *((unsigned long *)&__m256i_result[2]) = 0x0707feb60707c7eb;
  *((unsigned long *)&__m256i_result[1]) = 0x0707feb60707b7d0;
  *((unsigned long *)&__m256i_result[0]) = 0x0707feb60707c7eb;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1111111111111111;
  *((unsigned long *)&__m256i_result[2]) = 0x1111111111111111;
  *((unsigned long *)&__m256i_result[1]) = 0x1111111111111111;
  *((unsigned long *)&__m256i_result[0]) = 0x1111111111111111;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffa3;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000165a;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffa3;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000165a;
  *((unsigned long *)&__m256i_result[3]) = 0x1818ffff1818ffa3;
  *((unsigned long *)&__m256i_result[2]) = 0x181818181818185a;
  *((unsigned long *)&__m256i_result[1]) = 0x1818ffff1818ffa3;
  *((unsigned long *)&__m256i_result[0]) = 0x181818181818185a;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1c1c1c1c1c1c1c1c;
  *((unsigned long *)&__m256i_result[2]) = 0x1c1c1c1c1c1c1c1c;
  *((unsigned long *)&__m256i_result[1]) = 0x1c1c1c1c1c1c1c1c;
  *((unsigned long *)&__m256i_result[0]) = 0x1c1c1c1c1c1c1c1c;
  __m256i_out = __lasx_xvmaxi_bu (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xeffc000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xf064c6098d214127;
  *((unsigned long *)&__m256i_op0[1]) = 0xeffc000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xf064c6098d214127;
  *((unsigned long *)&__m256i_result[3]) = 0xeffc001800180018;
  *((unsigned long *)&__m256i_result[2]) = 0xf064c6098d214127;
  *((unsigned long *)&__m256i_result[1]) = 0xeffc001800180018;
  *((unsigned long *)&__m256i_result[0]) = 0xf064c6098d214127;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000003;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000003;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000100010003;
  *((unsigned long *)&__m256i_result[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010003;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0007000700070007;
  *((unsigned long *)&__m256i_result[2]) = 0x0007000700070007;
  *((unsigned long *)&__m256i_result[1]) = 0x0007000700070007;
  *((unsigned long *)&__m256i_result[0]) = 0x0007000700070007;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0018001800180018;
  *((unsigned long *)&__m256i_result[2]) = 0x0018001800180018;
  *((unsigned long *)&__m256i_result[1]) = 0x0018001800180018;
  *((unsigned long *)&__m256i_result[0]) = 0x0018001800180018;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[3]) = 0x0017001700176d6d;
  *((unsigned long *)&__m256i_result[2]) = 0x0017001700176d6d;
  *((unsigned long *)&__m256i_result[1]) = 0x0017001700176d6d;
  *((unsigned long *)&__m256i_result[0]) = 0x0017001700176d6d;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x001fffffffe00000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x001fffffffe00000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x001fffffffe00011;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x001fffffffe00011;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvmaxi_hu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001400000014;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001400000014;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001400000014;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001400000014;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000e00000080;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000e00000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000e00000080;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000e00000080;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000fd0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000fd0000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001b0000001b;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001b00fd0000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001b0000001b;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001b00fd0000;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000007aff7c00;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffd017d00;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000007aff7c00;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffd017d00;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000c7aff7c00;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffd017d00;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000c7aff7c00;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffd017d00;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001f0000001f;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001f0000ffff;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000300000003;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000300000003;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000300000003;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000300000003;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1010101010001000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x1010101000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x1010101010001000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[1]) = 0x101010100000000e;
  *((unsigned long *)&__m256i_result[0]) = 0x000000ff000000ff;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000007ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000007ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000000007ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000007ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001e0007ffff;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000fd;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000400000004;
  *((unsigned long *)&__m256i_result[2]) = 0x00000004000000fd;
  *((unsigned long *)&__m256i_result[1]) = 0x00000004000000fe;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000400000004;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000001f;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000001f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000001f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000001f;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000010;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001700000017;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001700000017;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001700000017;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001700000017;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x07fee332883f86b0;
  *((unsigned long *)&__m256i_op0[2]) = 0x07fed3c8f7ad28d0;
  *((unsigned long *)&__m256i_op0[1]) = 0x07fee332883f86b0;
  *((unsigned long *)&__m256i_op0[0]) = 0x07fed3c8f7ad28d0;
  *((unsigned long *)&__m256i_result[3]) = 0x07fee332883f86b0;
  *((unsigned long *)&__m256i_result[2]) = 0x07fed3c8f7ad28d0;
  *((unsigned long *)&__m256i_result[1]) = 0x07fee332883f86b0;
  *((unsigned long *)&__m256i_result[0]) = 0x07fed3c8f7ad28d0;
  __m256i_out = __lasx_xvmaxi_wu (__m256i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000001e;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000001e;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_op0[2]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_op0[1]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_op0[0]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_result[3]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_result[2]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_result[1]) = 0x1c1b1a191c1b1a19;
  *((unsigned long *)&__m256i_result[0]) = 0x1c1b1a191c1b1a19;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000003f;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffffffff;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000001c;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000001c;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000001c;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000001c;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000005;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000005;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000005;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000005;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000600000006;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffffe;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000012;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000012;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000007;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000000b;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000000b;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000000b;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000000b;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000013;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000013;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000013;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000013;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000014;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000014;
  __m256i_out = __lasx_xvmaxi_du (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
