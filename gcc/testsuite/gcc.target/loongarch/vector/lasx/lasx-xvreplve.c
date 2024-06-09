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
  int_op1 = 0x0000001b3c4c0a5c;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffefb;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffefb;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000fe;
  int_op1 = 0x0000000059815d00;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000000fe;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[2]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[1]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[0]) = 0x555555ab555555ab;
  int_op1 = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[2]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[1]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[0]) = 0x555555ab555555ab;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000012e2110;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[2]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[1]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_result[0]) = 0x1010101010101010;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000003f;
  int_op1 = 0x0000000000000400;
  *((unsigned long *)&__m256i_result[3]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[2]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[1]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[0]) = 0x003f003f003f003f;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_op0[2]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_op0[1]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_op0[0]) = 0x003f003f003f003f;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[2]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[1]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_result[0]) = 0x003f003f003f003f;
  __m256i_out = __lasx_xvreplve_w (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000003f0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000003f0000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xe161616161616161;
  *((unsigned long *)&__m256i_op0[2]) = 0xe161616161614e60;
  *((unsigned long *)&__m256i_op0[1]) = 0xe161616161616161;
  *((unsigned long *)&__m256i_op0[0]) = 0xe161616161614e60;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xe161616161614e60;
  *((unsigned long *)&__m256i_result[2]) = 0xe161616161614e60;
  *((unsigned long *)&__m256i_result[1]) = 0xe161616161614e60;
  *((unsigned long *)&__m256i_result[0]) = 0xe161616161614e60;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000080;
  int_op1 = 0x00000000000000ac;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000080;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000400;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff00d5007f00ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xff00ffffff00ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xff00d5007f00ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00ffffff00ffff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_w (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000020202020;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000020202020;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[2]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_result[0]) = 0x2020202020202020;
  __m256i_out = __lasx_xvreplve_w (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffff7fffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffff7fffff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xc192181230000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xc192181230000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000ff00ff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000ff00ff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000ff00ff;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fefffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fefffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x7fef7fef7fef7fef;
  *((unsigned long *)&__m256i_result[2]) = 0x7fef7fef7fef7fef;
  *((unsigned long *)&__m256i_result[1]) = 0x7fef7fef7fef7fef;
  *((unsigned long *)&__m256i_result[0]) = 0x7fef7fef7fef7fef;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffff00ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffff00ffffffff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fe37fe3001d001d;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff7fff7fff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fe37fe3001d001d;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff7fff0000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x007f010700c70106;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x007f010700c70106;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0106010601060106;
  *((unsigned long *)&__m256i_result[2]) = 0x0106010601060106;
  *((unsigned long *)&__m256i_result[1]) = 0x0106010601060106;
  *((unsigned long *)&__m256i_result[0]) = 0x0106010601060106;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_h (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve_w (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000003fff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000040;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000001010101;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000404;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000001010101;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000404;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0404040404040404;
  *((unsigned long *)&__m256i_result[2]) = 0x0404040404040404;
  *((unsigned long *)&__m256i_result[1]) = 0x0404040404040404;
  *((unsigned long *)&__m256i_result[0]) = 0x0404040404040404;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000800080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000800080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000202;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000202;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x000000003ddc5dac;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_d (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000200000002;
  int_op1 = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvreplve_b (__m256i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
