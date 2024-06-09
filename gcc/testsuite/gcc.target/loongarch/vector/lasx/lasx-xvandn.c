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

  *((unsigned long *)&__m256i_op0[3]) = 0x1828f0e09bad7249;
  *((unsigned long *)&__m256i_op0[2]) = 0x07ffc1b723953cec;
  *((unsigned long *)&__m256i_op0[1]) = 0x61f2e9b333aab104;
  *((unsigned long *)&__m256i_op0[0]) = 0x6bf742aa0d7856a0;
  *((unsigned long *)&__m256i_op1[3]) = 0x000019410000e69a;
  *((unsigned long *)&__m256i_op1[2]) = 0xf259905a09c23be0;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000883a00000f20;
  *((unsigned long *)&__m256i_op1[0]) = 0x6d3c2d3a89167aeb;
  *((unsigned long *)&__m256i_result[3]) = 0x0000090100008492;
  *((unsigned long *)&__m256i_result[2]) = 0xf000104808420300;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000e20;
  *((unsigned long *)&__m256i_result[0]) = 0x04082d108006284b;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xfffdfffdfffdfffd;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1020102010201020;
  *((unsigned long *)&__m256i_op0[2]) = 0x1020102010201020;
  *((unsigned long *)&__m256i_op0[1]) = 0x1020102010201020;
  *((unsigned long *)&__m256i_op0[0]) = 0x1020102010201020;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xefdfefdf00000000;
  *((unsigned long *)&__m256i_result[2]) = 0xefdfefdfefdfefdf;
  *((unsigned long *)&__m256i_result[1]) = 0xefdfefdf00000000;
  *((unsigned long *)&__m256i_result[0]) = 0xefdfefdfefdfefdf;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000008;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op1[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op1[1]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op1[0]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000d6d6d;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0fff0fff0fff0fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0fff0fff0fff0fff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000010101010101;
  *((unsigned long *)&__m256i_op1[2]) = 0x0101000000010000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000010101010101;
  *((unsigned long *)&__m256i_op1[0]) = 0x0101000000010000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvandn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
