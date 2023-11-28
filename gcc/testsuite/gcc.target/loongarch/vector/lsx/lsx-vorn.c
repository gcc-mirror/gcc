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

  *((unsigned long*)& __m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long*)& __m128i_op0[0]) = 0x000100010001fffe;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x00d3012b015700bb;
  *((unsigned long*)& __m128i_op1[0]) = 0x00010000ffca0070;
  *((unsigned long*)& __m128i_result[1]) = 0xff2cfed4fea8ff44;
  *((unsigned long*)& __m128i_result[0]) = 0xfffeffff0035ff8f;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00fe00fe00fe00fe;
  *((unsigned long*)& __m128i_op0[0]) = 0x00fe00fe00fe0045;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x00fe00fe00fe00fe;
  *((unsigned long*)& __m128i_result[0]) = 0x00fe00fe00fe0045;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000010000010101;
  *((unsigned long*)& __m128i_op0[0]) = 0x0101000001000100;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000010000010101;
  *((unsigned long*)& __m128i_op1[0]) = 0x0101000001000100;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000002;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000002;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x33f5c2d7d975d7fe;
  *((unsigned long*)& __m128i_op0[0]) = 0xe4423f7b769f8ffe;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x33f5c2d7d975d7fe;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00050eb00000fffa;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000f8a50000f310;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00bbfff7fffffff7;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffff008ff820;
  *((unsigned long*)& __m128i_op1[1]) = 0x0001000100010012;
  *((unsigned long*)& __m128i_op1[0]) = 0x00000000ffe1ffc0;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffff009ff83f;
  __m128i_out = __lsx_vorn_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
