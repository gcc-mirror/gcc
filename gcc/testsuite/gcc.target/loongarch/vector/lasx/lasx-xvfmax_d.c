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
  *((unsigned long *)&__m256d_op1[1]) = 0x000000040000fff8;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x000000040000fff8;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000008000000080;
  *((unsigned long *)&__m256d_op1[3]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256d_op1[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256d_op1[1]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256d_op1[0]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256d_result[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256d_result[0]) = 0x45c5c5c545c5c5c5;
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000004290;
  *((unsigned long *)&__m256d_op0[2]) = 0x00000000002a96ba;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000004290;
  *((unsigned long *)&__m256d_op0[0]) = 0x00000000002a96ba;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000083f95466;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0101010100005400;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000004290;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000083f95466;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000004290;
  *((unsigned long *)&__m256d_result[0]) = 0x0101010100005400;
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffff00000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
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
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0101000101010001;
  __m256d_out = __lasx_xvfmax_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0x0001000000000000;
  *((unsigned long *)&__m256d_op1[2]) = 0x0000000100010001;
  *((unsigned long *)&__m256d_op1[1]) = 0x0001000000000000;
  *((unsigned long *)&__m256d_op1[0]) = 0x0000000100010001;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
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
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
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
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0200000202000002;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0200000202000002;
  *((unsigned long *)&__m256d_op1[3]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_op1[2]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_op1[1]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_op1[0]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0101000101010001;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0101000101010001;
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
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
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
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
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0xfffeb6839ffffd80;
  *((unsigned long *)&__m256d_op1[2]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256d_op1[1]) = 0xfffeb6839ffffd80;
  *((unsigned long *)&__m256d_op1[0]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfmin_d (__m256d_op0, __m256d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  return 0;
}
