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

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x000000040000fff8;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x000000040000fff8;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0xffffffffffff8001;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000018;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000018;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000018;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000018;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0002000000020000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0002000000010000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0002000000020000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0002000000010000;
  *((unsigned long *)&__m256d_op1[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0xfff0000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0xfff0000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0002000000020000;
  *((unsigned long *)&__m256d_result[2]) = 0xfff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0002000000020000;
  *((unsigned long *)&__m256d_result[0]) = 0xfff0000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000001;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256d_op1[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256d_op1[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256d_op1[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmaxa_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256d_op1[2]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256d_op1[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256d_op1[0]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000100;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffb2f600006f48;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffb2f600006f48;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x00000000000000ff;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_op0[2]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256d_op1[3]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_op1[2]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_result[2]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7efefefe80ffffff;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0087ff87f807ff87;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0087ff87f807ff87;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmina_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  return 0;
}
