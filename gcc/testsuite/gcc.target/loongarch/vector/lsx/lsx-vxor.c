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

  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x00000000f4012ceb;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x00000000f4012ceb;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x3ff0000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x40f3fa0000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x3ff0000000000001;
  *((unsigned long*)& __m128i_result[0]) = 0x40f3fa0000000000;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000080000068;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000080000068;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000ffff53d9;
  *((unsigned long*)& __m128i_op0[0]) = 0xff000001ffff9515;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffff0001ffff9514;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffff0000ac26;
  *((unsigned long*)& __m128i_result[0]) = 0x00ff000000000001;
  __m128i_out = __lsx_vxor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
