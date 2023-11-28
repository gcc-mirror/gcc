/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lsxintrin.h>

int main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i=1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long*)& __m128i_op0[1]) = 0x00000000fff8fff8;
  *((unsigned long*)& __m128i_op0[0]) = 0x00000000fff80000;
  *((unsigned long*)& __m128i_op1[1]) = 0x00000000fff8fff8;
  *((unsigned long*)& __m128i_op1[0]) = 0x00000000fff80000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffff00070007;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffff0007ffff;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xce23d33e43d9736c;
  *((unsigned long*)& __m128i_op0[0]) = 0x63b2ac27aa076aeb;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x31dc2cc1bc268c93;
  *((unsigned long*)& __m128i_result[0]) = 0x9c4d53d855f89514;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x000000000000000c;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xfffffffffffffff3;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000400080003fff;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000bc2000007e04;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000400080003fff;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000bc2000007e04;
  *((unsigned long*)& __m128i_result[1]) = 0xffffbfff7fffc000;
  *((unsigned long*)& __m128i_result[0]) = 0xffff43dfffff81fb;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x021b7d24c9678a35;
  *((unsigned long*)& __m128i_op0[0]) = 0x030298a6a1030a49;
  *((unsigned long*)& __m128i_op1[1]) = 0x5252525252525252;
  *((unsigned long*)& __m128i_op1[0]) = 0x5252525252525252;
  *((unsigned long*)& __m128i_result[1]) = 0xada4808924882588;
  *((unsigned long*)& __m128i_result[0]) = 0xacad25090caca5a4;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0xfffefffe0000ff18;
  *((unsigned long*)& __m128i_op1[1]) = 0xffff000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x0000ffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0x0001000100000000;
  __m128i_out = __lsx_vnor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
