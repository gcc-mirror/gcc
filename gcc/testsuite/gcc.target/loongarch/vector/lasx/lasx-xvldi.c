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

  *((unsigned long *)&__m256i_result[3]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001000000010;
  __m256i_out = __lasx_xvldi (-4080);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0xfebcfebcfebcfebc;
  *((unsigned long *)&__m256i_result[2]) = 0xfebcfebcfebcfebc;
  *((unsigned long *)&__m256i_result[1]) = 0xfebcfebcfebcfebc;
  *((unsigned long *)&__m256i_result[0]) = 0xfebcfebcfebcfebc;
  __m256i_out = __lasx_xvldi (1724);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x3fd1000000000000;
  __m256i_out = __lasx_xvldi (-943);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[2]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[1]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[0]) = 0xff1cff1cff1cff1c;
  __m256i_out = __lasx_xvldi (1820);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[1]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7200000072000000;
  __m256i_out = __lasx_xvldi (-3214);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0xffffff1dffffff1d;
  *((unsigned long *)&__m256i_result[2]) = 0xffffff1dffffff1d;
  *((unsigned long *)&__m256i_result[1]) = 0xffffff1dffffff1d;
  *((unsigned long *)&__m256i_result[0]) = 0xffffff1dffffff1d;
  __m256i_out = __lasx_xvldi (2845);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001000000010;
  __m256i_out = __lasx_xvldi (-4080);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x3fd1000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x3fd1000000000000;
  __m256i_out = __lasx_xvldi (-943);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_result[3]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[1]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7200000072000000;
  __m256i_out = __lasx_xvldi (-3214);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
