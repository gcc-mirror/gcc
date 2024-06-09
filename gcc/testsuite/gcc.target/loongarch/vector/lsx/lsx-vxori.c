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
  *((unsigned long*)& __m128i_result[1]) = 0x0404040404040404;
  *((unsigned long*)& __m128i_result[0]) = 0x0404040404040404;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0x4);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000001000100;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000001000100;
  *((unsigned long*)& __m128i_result[1]) = 0x5a5a5a5a5b5a5b5a;
  *((unsigned long*)& __m128i_result[0]) = 0x5a5a5a5a5b5a5b5a;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0x5a);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xe3e3e3e3e3e3e3e3;
  *((unsigned long*)& __m128i_result[0]) = 0xe3e3e3e3e3e3e3e3;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0xe3);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0020002000200020;
  *((unsigned long*)& __m128i_result[1]) = 0x9a9a9a9a9a9a9a9a;
  *((unsigned long*)& __m128i_result[0]) = 0x9aba9aba9aba9aba;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0x9a);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x9090909090909090;
  *((unsigned long*)& __m128i_result[0]) = 0x9090909090909090;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0x90);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000b81c8382;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000077af9450;
  *((unsigned long*)& __m128i_result[1]) = 0xf1f1f1f149ed7273;
  *((unsigned long*)& __m128i_result[0]) = 0xf1f1f1f1865e65a1;
  __m128i_out = __lsx_vxori_b(__m128i_op0,0xf1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
