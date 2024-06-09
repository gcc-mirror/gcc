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
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x6c6c6c6c6c6c6c6c;
  *((unsigned long *)&__m256i_result[2]) = 0x6c6c6c6c6c6c6c6c;
  *((unsigned long *)&__m256i_result[1]) = 0x6c6c6c6c6c6c6c6c;
  *((unsigned long *)&__m256i_result[0]) = 0x6c6c6c6c6c6c6c6c;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x6c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffff00fffffff0;
  *((unsigned long *)&__m256i_result[3]) = 0x9f9f9f9f9f9f9f9f;
  *((unsigned long *)&__m256i_result[2]) = 0x9f9f9f9fffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x9f9f9f9f9f9f9f9f;
  *((unsigned long *)&__m256i_result[0]) = 0xffffff9fffffffff;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x9f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x6a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffbdff3cffbdff44;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffbdff3cffbdff44;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffff7effffff46;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffff7effffff46;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x42);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xbfbfbfbfbfbfbfbf;
  *((unsigned long *)&__m256i_result[2]) = 0xbfbfbfbfbfbfbfbf;
  *((unsigned long *)&__m256i_result[1]) = 0xbfbfbfbfbfbfbfbf;
  *((unsigned long *)&__m256i_result[0]) = 0xbfbfbfbfbfbfbfbf;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0xbf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x2c2c2c2c2c2c2c2c;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x2c2c2c2c2c2c2c2c;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x2c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x5252525252525252;
  *((unsigned long *)&__m256i_result[2]) = 0x5252525252525252;
  *((unsigned long *)&__m256i_result[1]) = 0x5252525252525252;
  *((unsigned long *)&__m256i_result[0]) = 0x5252525252525252;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x52);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x3f8000003f800000;
  *((unsigned long *)&__m256i_op0[2]) = 0x3f8000003f800000;
  *((unsigned long *)&__m256i_op0[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m256i_op0[0]) = 0x3f8000003f800000;
  *((unsigned long *)&__m256i_result[3]) = 0x7fe363637fe36363;
  *((unsigned long *)&__m256i_result[2]) = 0x7fe363637fe36363;
  *((unsigned long *)&__m256i_result[1]) = 0x7fe363637fe36363;
  *((unsigned long *)&__m256i_result[0]) = 0x7fe363637fe36363;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x63);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xfefefefe3f800000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xfefefefe3f800000;
  *((unsigned long *)&__m256i_result[3]) = 0xe0e0e0e0e0e0e0e0;
  *((unsigned long *)&__m256i_result[2]) = 0xfefefefeffe0e0e0;
  *((unsigned long *)&__m256i_result[1]) = 0xe0e0e0e0e0e0e0e0;
  *((unsigned long *)&__m256i_result[0]) = 0xfefefefeffe0e0e0;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0xe0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x6b6b6b6b6b6b6b6b;
  *((unsigned long *)&__m256i_result[2]) = 0x6b6b6b6b6b6b6b6b;
  *((unsigned long *)&__m256i_result[1]) = 0x6b6b6b6b6b6b6b6b;
  *((unsigned long *)&__m256i_result[0]) = 0x6b6b6b6b6b6b6b6b;
  __m256i_out = __lasx_xvori_b (__m256i_op0, 0x6b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
