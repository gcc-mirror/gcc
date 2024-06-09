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
  *((unsigned long *)&__m256i_op0[2]) = 0x0101000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0101000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x4370100000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x4370100000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_op0[2]) = 0x2020202020206431;
  *((unsigned long *)&__m256i_op0[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_op0[0]) = 0x2020202020206431;
  *((unsigned long *)&__m256d_result[3]) = 0x43c0101010101010;
  *((unsigned long *)&__m256d_result[2]) = 0x43c0101010101032;
  *((unsigned long *)&__m256d_result[1]) = 0x43c0101010101010;
  *((unsigned long *)&__m256d_result[0]) = 0x43c0101010101032;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x40efffe09fa88260;
  *((unsigned long *)&__m256i_op0[2]) = 0x6b07ca8e013fbf01;
  *((unsigned long *)&__m256i_op0[1]) = 0x40efffe09fa7e358;
  *((unsigned long *)&__m256i_op0[0]) = 0x80ce32be3e827f00;
  *((unsigned long *)&__m256d_result[3]) = 0x43d03bfff827ea21;
  *((unsigned long *)&__m256d_result[2]) = 0x43dac1f2a3804ff0;
  *((unsigned long *)&__m256d_result[1]) = 0x43d03bfff827e9f9;
  *((unsigned long *)&__m256d_result[0]) = 0x43e019c657c7d050;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x43f0000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m256d_result[3]) = 0x41f0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x41f0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x41f0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x41f0000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[2]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[1]) = 0x0202010202020102;
  *((unsigned long *)&__m256i_op0[0]) = 0x0202010202020102;
  *((unsigned long *)&__m256d_result[3]) = 0x4380100810101008;
  *((unsigned long *)&__m256d_result[2]) = 0x4380100810101008;
  *((unsigned long *)&__m256d_result[1]) = 0x4380100810101008;
  *((unsigned long *)&__m256d_result[0]) = 0x4380100810101008;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x41f0000000000000;
  __m256d_out = __lasx_xvffint_d_lu (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffbf7f00007fff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffe651ffffbfff;
  *((int *)&__m256_result[7]) = 0x4f800000;
  *((int *)&__m256_result[6]) = 0x4f800000;
  *((int *)&__m256_result[5]) = 0x4f7fffbf;
  *((int *)&__m256_result[4]) = 0x46fffe00;
  *((int *)&__m256_result[3]) = 0x4f800000;
  *((int *)&__m256_result[2]) = 0x4f800000;
  *((int *)&__m256_result[1]) = 0x4f7fffe6;
  *((int *)&__m256_result[0]) = 0x4f7fffc0;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffffffefd;
  *((unsigned long *)&__m256i_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((int *)&__m256_result[7]) = 0x4b808080;
  *((int *)&__m256_result[6]) = 0x4b808080;
  *((int *)&__m256_result[5]) = 0x4f800000;
  *((int *)&__m256_result[4]) = 0x4f7fffff;
  *((int *)&__m256_result[3]) = 0x4b808080;
  *((int *)&__m256_result[2]) = 0x4b808080;
  *((int *)&__m256_result[1]) = 0x4f800000;
  *((int *)&__m256_result[0]) = 0x4f800000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((int *)&__m256_result[7]) = 0x00000000;
  *((int *)&__m256_result[6]) = 0x00000000;
  *((int *)&__m256_result[5]) = 0x00000000;
  *((int *)&__m256_result[4]) = 0x00000000;
  *((int *)&__m256_result[3]) = 0x00000000;
  *((int *)&__m256_result[2]) = 0x00000000;
  *((int *)&__m256_result[1]) = 0x00000000;
  *((int *)&__m256_result[0]) = 0x00000000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((int *)&__m256_result[7]) = 0x00000000;
  *((int *)&__m256_result[6]) = 0x00000000;
  *((int *)&__m256_result[5]) = 0x00000000;
  *((int *)&__m256_result[4]) = 0x00000000;
  *((int *)&__m256_result[3]) = 0x00000000;
  *((int *)&__m256_result[2]) = 0x00000000;
  *((int *)&__m256_result[1]) = 0x00000000;
  *((int *)&__m256_result[0]) = 0x00000000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000008;
  *((int *)&__m256_result[7]) = 0x00000000;
  *((int *)&__m256_result[6]) = 0x41000000;
  *((int *)&__m256_result[5]) = 0x00000000;
  *((int *)&__m256_result[4]) = 0x41000000;
  *((int *)&__m256_result[3]) = 0x00000000;
  *((int *)&__m256_result[2]) = 0x41000000;
  *((int *)&__m256_result[1]) = 0x00000000;
  *((int *)&__m256_result[0]) = 0x41000000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000020;
  *((int *)&__m256_result[7]) = 0x00000000;
  *((int *)&__m256_result[6]) = 0x42800000;
  *((int *)&__m256_result[5]) = 0x00000000;
  *((int *)&__m256_result[4]) = 0x42000000;
  *((int *)&__m256_result[3]) = 0x00000000;
  *((int *)&__m256_result[2]) = 0x42800000;
  *((int *)&__m256_result[1]) = 0x00000000;
  *((int *)&__m256_result[0]) = 0x42000000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((int *)&__m256_result[7]) = 0x00000000;
  *((int *)&__m256_result[6]) = 0x00000000;
  *((int *)&__m256_result[5]) = 0x00000000;
  *((int *)&__m256_result[4]) = 0x00000000;
  *((int *)&__m256_result[3]) = 0x00000000;
  *((int *)&__m256_result[2]) = 0x00000000;
  *((int *)&__m256_result[1]) = 0x00000000;
  *((int *)&__m256_result[0]) = 0x00000000;
  __m256_out = __lasx_xvffint_s_wu (__m256i_op0);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);

  return 0;
}
