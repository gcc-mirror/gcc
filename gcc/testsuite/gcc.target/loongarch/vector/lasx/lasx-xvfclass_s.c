/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
/* { dg-timeout 500 } */
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

  *((int *)&__m256_op0[7]) = 0xfffffff8;
  *((int *)&__m256_op0[6]) = 0xffffff08;
  *((int *)&__m256_op0[5]) = 0x00ff00f8;
  *((int *)&__m256_op0[4]) = 0x00ffcff8;
  *((int *)&__m256_op0[3]) = 0xfffffff8;
  *((int *)&__m256_op0[2]) = 0xffffff08;
  *((int *)&__m256_op0[1]) = 0x00ff00f8;
  *((int *)&__m256_op0[0]) = 0x00ffcff8;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000008000000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000008000000080;
  __m256i_out = __lasx_xvfclass_s (__m256_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((int *)&__m256_op0[7]) = 0xffffffff;
  *((int *)&__m256_op0[6]) = 0xffffffff;
  *((int *)&__m256_op0[5]) = 0xffffffff;
  *((int *)&__m256_op0[4]) = 0xffffffff;
  *((int *)&__m256_op0[3]) = 0xffffffff;
  *((int *)&__m256_op0[2]) = 0xffffffff;
  *((int *)&__m256_op0[1]) = 0xffffffff;
  *((int *)&__m256_op0[0]) = 0xffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000200000002;
  __m256i_out = __lasx_xvfclass_s (__m256_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((int *)&__m256_op0[7]) = 0x00000000;
  *((int *)&__m256_op0[6]) = 0x00000000;
  *((int *)&__m256_op0[5]) = 0x00000000;
  *((int *)&__m256_op0[4]) = 0x00000000;
  *((int *)&__m256_op0[3]) = 0x00000000;
  *((int *)&__m256_op0[2]) = 0x00000000;
  *((int *)&__m256_op0[1]) = 0x00000000;
  *((int *)&__m256_op0[0]) = 0x00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000020000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000020000000200;
  *((unsigned long *)&__m256i_result[1]) = 0x0000020000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000020000000200;
  __m256i_out = __lasx_xvfclass_s (__m256_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((int *)&__m256_op0[7]) = 0x00000000;
  *((int *)&__m256_op0[6]) = 0x00000000;
  *((int *)&__m256_op0[5]) = 0x000000ff;
  *((int *)&__m256_op0[4]) = 0x000000ff;
  *((int *)&__m256_op0[3]) = 0x00000000;
  *((int *)&__m256_op0[2]) = 0x00000000;
  *((int *)&__m256_op0[1]) = 0x000000ff;
  *((int *)&__m256_op0[0]) = 0x000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000020000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000010000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000020000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000010000000100;
  __m256i_out = __lasx_xvfclass_s (__m256_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((int *)&__m256_op0[7]) = 0xfffffffb;
  *((int *)&__m256_op0[6]) = 0xfffffffb;
  *((int *)&__m256_op0[5]) = 0xfffffffb;
  *((int *)&__m256_op0[4]) = 0xfffffffb;
  *((int *)&__m256_op0[3]) = 0xfffffffb;
  *((int *)&__m256_op0[2]) = 0xfffffffb;
  *((int *)&__m256_op0[1]) = 0xfffffffb;
  *((int *)&__m256_op0[0]) = 0xfffffffb;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000200000002;
  __m256i_out = __lasx_xvfclass_s (__m256_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
