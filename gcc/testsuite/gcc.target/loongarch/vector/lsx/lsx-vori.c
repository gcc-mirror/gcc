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
  *((unsigned long*)& __m128i_result[1]) = 0x8282828282828282;
  *((unsigned long*)& __m128i_result[0]) = 0x8282828282828282;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x82);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x7505853d654185f5;
  *((unsigned long*)& __m128i_op0[0]) = 0x01010000fefe0101;
  *((unsigned long*)& __m128i_result[1]) = 0x7545c57d6541c5f5;
  *((unsigned long*)& __m128i_result[0]) = 0x41414040fefe4141;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x40);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000c2f90000bafa;
  *((unsigned long*)& __m128i_op0[0]) = 0x8000c2fa8000c2fa;
  *((unsigned long*)& __m128i_result[1]) = 0x7474f6fd7474fefe;
  *((unsigned long*)& __m128i_result[0]) = 0xf474f6fef474f6fe;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x74);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x3d3d3d3d3d3d3d3d;
  *((unsigned long*)& __m128i_result[0]) = 0x3d3d3d3d3d3d3d3d;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x3d);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xfffffacdb6dbecac;
  *((unsigned long*)& __m128i_op0[0]) = 0x1f5533a694f902c0;
  *((unsigned long*)& __m128i_result[1]) = 0xfffffadffedbfefe;
  *((unsigned long*)& __m128i_result[0]) = 0x5f5f7bfedefb5ada;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x5a);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x38);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0d1202e19235e2bc;
  *((unsigned long*)& __m128i_op0[0]) = 0xea38e0f75f6e56d1;
  *((unsigned long*)& __m128i_result[1]) = 0x2f3626e7b637e6be;
  *((unsigned long*)& __m128i_result[0]) = 0xee3ee6f77f6e76f7;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x26);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0003c853c843c87e;
  *((unsigned long*)& __m128i_op0[0]) = 0x0003c853c843c87e;
  *((unsigned long*)& __m128i_result[1]) = 0xd6d7ded7ded7defe;
  *((unsigned long*)& __m128i_result[0]) = 0xd6d7ded7ded7defe;
  __m128i_out = __lsx_vori_b(__m128i_op0,0xd6);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0xfffe0000fffe0000;
  *((unsigned long*)& __m128i_result[1]) = 0x7777777777777777;
  *((unsigned long*)& __m128i_result[0]) = 0xffff7777ffff7777;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x77);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x55);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x8000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x8000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xd454545454545454;
  *((unsigned long*)& __m128i_result[0]) = 0xd454545454545454;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x54);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long*)& __m128i_result[0]) = 0x4f4f4f4f4f4f4f4f;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x4f);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x8a8a8a8a8a8a8a8a;
  *((unsigned long*)& __m128i_result[0]) = 0x8a8a8a8a8a8a8a8a;
  __m128i_out = __lsx_vori_b(__m128i_op0,0x8a);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
