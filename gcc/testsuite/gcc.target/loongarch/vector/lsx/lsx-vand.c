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
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x03574e3a62407e03;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000001010000;
  *((unsigned long*)& __m128i_result[1]) = 0x03574e3a62407e03;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[1]) = 0x001fffff001fffff;
  *((unsigned long*)& __m128i_op1[0]) = 0x001fffff001fffff;
  *((unsigned long*)& __m128i_result[1]) = 0x001fffff001fffff;
  *((unsigned long*)& __m128i_result[0]) = 0x001fffff001fffff;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000003dffc2;
  *((unsigned long*)& __m128i_op0[0]) = 0x00000000003dffc2;
  *((unsigned long*)& __m128i_op1[1]) = 0x0008000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000ffff53d9;
  *((unsigned long*)& __m128i_op0[0]) = 0xffff0001ffff9515;
  *((unsigned long*)& __m128i_op1[1]) = 0x00000000ffff53d9;
  *((unsigned long*)& __m128i_op1[0]) = 0xffff0001ffff9515;
  *((unsigned long*)& __m128i_result[1]) = 0x00000000ffff53d9;
  *((unsigned long*)& __m128i_result[0]) = 0xffff0001ffff9515;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x67eb85af0000b000;
  *((unsigned long*)& __m128i_op0[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long*)& __m128i_op1[1]) = 0x67eb85af0000b000;
  *((unsigned long*)& __m128i_op1[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long*)& __m128i_result[1]) = 0x67eb85af0000b000;
  *((unsigned long*)& __m128i_result[0]) = 0xc8847ef6ed3f2000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0313100003131000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0313100003131000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000200000002;
  *((unsigned long*)& __m128i_op1[1]) = 0x0007000000050000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0003000100010001;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00007a8000000480;
  *((unsigned long*)& __m128i_op0[0]) = 0x00000485000004cc;
  *((unsigned long*)& __m128i_op1[1]) = 0x0a0000000a000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0a0000000a000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vand_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
