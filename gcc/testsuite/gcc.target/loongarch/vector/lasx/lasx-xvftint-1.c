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

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftint_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffcb423a587053;
  *((unsigned long *)&__m256d_op0[2]) = 0x6d46f43e71141b81;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffcb423a584528;
  *((unsigned long *)&__m256d_op0[0]) = 0x9bdf36c8d78158a1;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftint_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x386000003df80000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x386000003df80000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftint_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftint_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftint_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x555555553f800000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x555555553f800000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x43f0000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x43f0000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffc03b1fc5e050;
  *((unsigned long *)&__m256d_op0[2]) = 0x6a9e3fa2603a2000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffc03b1fc5e050;
  *((unsigned long *)&__m256d_op0[0]) = 0x6a9e3fa2603a2000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrne_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffff00000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000100000001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x7f70000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x7f70000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x7f70000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x7f70000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256d_op0[2]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256d_op0[1]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256d_op0[0]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x8000800080008000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrp_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00007f8000007f80;
  *((unsigned long *)&__m256d_op0[2]) = 0x00007f8000007f80;
  *((unsigned long *)&__m256d_op0[1]) = 0x00007f8000007f80;
  *((unsigned long *)&__m256d_op0[0]) = 0x00007f8000007f80;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x555555553f800000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x555555553f800000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00000000001c9880;
  *((unsigned long *)&__m256d_op0[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x00000000001c9880;
  *((unsigned long *)&__m256d_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrm_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x1828f0e09bad7249;
  *((unsigned long *)&__m256d_op0[2]) = 0x07ffc1b723953cec;
  *((unsigned long *)&__m256d_op0[1]) = 0x61f2e9b333aab104;
  *((unsigned long *)&__m256d_op0[0]) = 0x6bf742aa0d7856a0;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x7fffffffffffffff;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000ffff00010000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000ffff00010000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00ffffff1e9e9e9e;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffff9e9eb09e;
  *((unsigned long *)&__m256d_op0[1]) = 0x00ffffff1e9e9e9e;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffff9e9eb09e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000001e0007ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvftintrz_l_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
