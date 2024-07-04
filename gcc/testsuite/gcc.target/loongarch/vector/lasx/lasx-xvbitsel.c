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
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_op0[2]) = 0x001f001f02c442af;
  *((unsigned long *)&__m256i_op0[1]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_op0[0]) = 0x001f001f02c442af;
  *((unsigned long *)&__m256i_op1[3]) = 0x00fe01f000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x00fe01f000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xfffffffffefefeff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffff295329;
  *((unsigned long *)&__m256i_op2[1]) = 0xfffffffffefefeff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffff295329;
  *((unsigned long *)&__m256i_result[3]) = 0x00fe01f000010000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000c40086;
  *((unsigned long *)&__m256i_result[1]) = 0x00fe01f000010000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000c40086;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op1[3]) = 0xbe21000100000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000505300000000;
  *((unsigned long *)&__m256i_op1[1]) = 0xbe21000100000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000505300000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x41dfffffffc00000;
  *((unsigned long *)&__m256i_op2[2]) = 0xc1d75053f0000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x41dfffffffc00000;
  *((unsigned long *)&__m256i_op2[0]) = 0xc1d75053f0000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000100000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00005053000000ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00005053000000ff;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000040000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000040000;
  *((unsigned long *)&__m256i_op2[3]) = 0x00000e0000000e00;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0x00000e0000000e00;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000040000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000040000;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x8000000000000000;
  __m256i_out = __lasx_xvbitsel_v (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
