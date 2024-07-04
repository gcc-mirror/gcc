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
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000005e02;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000005e02;
  *((unsigned long *)&__m256i_result[3]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_result[2]) = 0xc2c2c2c2c2c29cc0;
  *((unsigned long *)&__m256i_result[1]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_result[0]) = 0xc2c2c2c2c2c29cc0;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xc2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1616161616161616;
  *((unsigned long *)&__m256i_op0[2]) = 0x161616167fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ffe16167f161616;
  *((unsigned long *)&__m256i_op0[0]) = 0x161616167fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xc7c7c7c7c7c7c7c7;
  *((unsigned long *)&__m256i_result[2]) = 0xc7c7c7c7ae2e2e2e;
  *((unsigned long *)&__m256i_result[1]) = 0xae2fc7c7aec7c7c7;
  *((unsigned long *)&__m256i_result[0]) = 0xc7c7c7c7ae2e2e2e;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xd1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x5353535353535353;
  *((unsigned long *)&__m256i_result[2]) = 0x5353535353535353;
  *((unsigned long *)&__m256i_result[1]) = 0x5353535353535353;
  *((unsigned long *)&__m256i_result[0]) = 0x5353535353535353;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x53);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x6d6d6d6d6d6d6d6d;
  *((unsigned long *)&__m256i_result[2]) = 0x6d6d6d6d6d6d6d6d;
  *((unsigned long *)&__m256i_result[1]) = 0x6d6d6d6d6d6d6d6d;
  *((unsigned long *)&__m256i_result[0]) = 0x6d6d6d6d6d6d6d6d;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x6d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x7171717171717171;
  *((unsigned long *)&__m256i_result[2]) = 0x8e8e8e8e8e8e8e8e;
  *((unsigned long *)&__m256i_result[1]) = 0x7171717171717171;
  *((unsigned long *)&__m256i_result[0]) = 0x8e8e8e8e8e8e8e8e;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x71);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x7575757575757575;
  *((unsigned long *)&__m256i_result[2]) = 0x7575757575757575;
  *((unsigned long *)&__m256i_result[1]) = 0x7575757575757575;
  *((unsigned long *)&__m256i_result[0]) = 0x7575757575757575;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x75);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xa4a4a4a4a4a4a4a4;
  *((unsigned long *)&__m256i_result[2]) = 0xa4a4a4a4a4a4a4a4;
  *((unsigned long *)&__m256i_result[1]) = 0xa4a4a4a4a4a4a4a4;
  *((unsigned long *)&__m256i_result[0]) = 0xa4a4a4a4a4a4a4a4;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xa4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xa1a1a1a1a1a1a1a1;
  *((unsigned long *)&__m256i_result[2]) = 0xa1a1a1a15e5e5e5e;
  *((unsigned long *)&__m256i_result[1]) = 0xa1a1a1a1a1a1a1a1;
  *((unsigned long *)&__m256i_result[0]) = 0xa1a1a1a15e5e5e5e;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xa1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x8d8d72728d8d7272;
  *((unsigned long *)&__m256i_result[2]) = 0x8d8d72728d8d8d8d;
  *((unsigned long *)&__m256i_result[1]) = 0x8d8d72728d8d7272;
  *((unsigned long *)&__m256i_result[0]) = 0x8d8d72728d8d8d8d;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x8d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xb3b3b3b3b3b3b3b3;
  *((unsigned long *)&__m256i_result[2]) = 0xb3b3b3b3b3b3b3b3;
  *((unsigned long *)&__m256i_result[1]) = 0xb3b3b3b3b3b3b3b3;
  *((unsigned long *)&__m256i_result[0]) = 0xb3b3b3b3b3b3b3b3;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x4c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffff800000;
  *((unsigned long *)&__m256i_op0[2]) = 0x007f0000ff807f81;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffff800000;
  *((unsigned long *)&__m256i_op0[0]) = 0x007f0000ff807f81;
  *((unsigned long *)&__m256i_result[3]) = 0x5d5d5d5d5d22a2a2;
  *((unsigned long *)&__m256i_result[2]) = 0xa2dda2a25d22dd23;
  *((unsigned long *)&__m256i_result[1]) = 0x5d5d5d5d5d22a2a2;
  *((unsigned long *)&__m256i_result[0]) = 0xa2dda2a25d22dd23;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xa2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xd3d3d3d3d3d3d3d3;
  *((unsigned long *)&__m256i_result[2]) = 0xd3d3d3d3d3d3d3d3;
  *((unsigned long *)&__m256i_result[1]) = 0xd3d3d3d3d3d3d3d3;
  *((unsigned long *)&__m256i_result[0]) = 0xd3d3d3d3d3d3d3d3;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0xd3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xfa15fa15fa15fa14;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xfa15fa15fa15fa14;
  *((unsigned long *)&__m256i_result[3]) = 0x8282828282828282;
  *((unsigned long *)&__m256i_result[2]) = 0x8768876887688769;
  *((unsigned long *)&__m256i_result[1]) = 0x8282828282828282;
  *((unsigned long *)&__m256i_result[0]) = 0x8768876887688769;
  __m256i_out = __lasx_xvxori_b (__m256i_op0, 0x7d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
