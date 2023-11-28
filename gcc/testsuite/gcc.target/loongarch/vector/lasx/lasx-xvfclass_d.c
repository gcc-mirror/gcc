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

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000010001;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000017f0000017d;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000010001;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000017f0000017f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000100;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000002;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000200;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0006000000040000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0002000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0006000000040000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0002000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000100;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xbf00bf00bf00bf00;
  *((unsigned long *)&__m256d_op0[2]) = 0xbf84bf00bf00bf0e;
  *((unsigned long *)&__m256d_op0[1]) = 0xbf00bf00bf00bf00;
  *((unsigned long *)&__m256d_op0[0]) = 0xbf84bf00bf00bf0e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000008;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffff00ffffff00;
  *((unsigned long *)&__m256d_op0[2]) = 0xff00000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffff00ffffff00;
  *((unsigned long *)&__m256d_op0[0]) = 0xff00000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000008;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x00000000ffff0001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x00000000ffff0001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000100;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000002;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000100;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00000000000000b7;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffefff80;
  *((unsigned long *)&__m256d_op0[1]) = 0x00000000000000b7;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffefff80;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000002;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000080;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000400000004000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000400000004000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000400000004000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000400000004000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000100;
  __m256i_out = __lasx_xvfclass_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
