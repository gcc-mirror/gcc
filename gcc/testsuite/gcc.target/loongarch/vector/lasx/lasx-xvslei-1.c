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
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, -3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000101ff01;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0001000000010000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00010013000100fe;
  *((unsigned long *)&__m256i_op0[1]) = 0x0001000000010000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00010013000100fb;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_b (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000101;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffff0000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x014200c200c200ae;
  *((unsigned long *)&__m256i_op0[2]) = 0x014200c200c200ae;
  *((unsigned long *)&__m256i_op0[1]) = 0x014200c200c200ae;
  *((unsigned long *)&__m256i_op0[0]) = 0x014200c200c200ae;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000c9;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000c9;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffff0000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff0000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffff8900000040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffff8900000040;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000ffff00000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffff000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffff000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffff000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffff000000000000;
  __m256i_out = __lasx_xvslei_h (__m256i_op0, -8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000460086;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000007f0079;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000f30028;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000df00ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfc2f3183ef7ffff7;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0001000100800000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1828f0e09bad7249;
  *((unsigned long *)&__m256i_op0[2]) = 0x07ffc1b723953cec;
  *((unsigned long *)&__m256i_op0[1]) = 0x61f2e9b333aab104;
  *((unsigned long *)&__m256i_op0[0]) = 0x6bf742aa0d7856a0;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, -15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_op0[2]) = 0xc6c6c6c68787878a;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_op0[0]) = 0x8787878a00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, 1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvslei_d (__m256i_op0, -3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}
