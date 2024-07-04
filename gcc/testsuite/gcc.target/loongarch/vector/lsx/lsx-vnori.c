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

  *((unsigned long*)& __m128i_op0[1]) = 0x00000000ffff0000;
  *((unsigned long*)& __m128i_op0[0]) = 0x00000000ffff0000;
  *((unsigned long*)& __m128i_result[1]) = 0xcccccccc0000cccc;
  *((unsigned long*)& __m128i_result[0]) = 0xcccccccc0000cccc;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0x33);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0xa6);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x3e035e51522f0799;
  *((unsigned long*)& __m128i_result[1]) = 0x9292929292929292;
  *((unsigned long*)& __m128i_result[0]) = 0x8090808280909002;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0x6d);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000000ffc2f;
  *((unsigned long*)& __m128i_op0[0]) = 0x00201df000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x3838383838300010;
  *((unsigned long*)& __m128i_result[0]) = 0x3818200838383838;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0xc7);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x2020202020202020;
  *((unsigned long*)& __m128i_op0[0]) = 0x2020202020207f7f;
  *((unsigned long*)& __m128i_result[1]) = 0x5d5d5d5d5d5d5d5d;
  *((unsigned long*)& __m128i_result[0]) = 0x5d5d5d5d5d5d0000;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0xa2);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x8080808080808080;
  *((unsigned long*)& __m128i_result[0]) = 0x8080808080808080;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0x7f);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long*)& __m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long*)& __m128i_result[1]) = 0x1313131313131313;
  *((unsigned long*)& __m128i_result[0]) = 0x1313131313131313;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0xec);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x9d9d9d9d9d9d9d9d;
  *((unsigned long*)& __m128i_result[0]) = 0x9d9d9d9d9d9d9d9d;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0x62);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00f525682ffd27f2;
  *((unsigned long*)& __m128i_op0[0]) = 0x00365c60317ff930;
  *((unsigned long*)& __m128i_result[1]) = 0xe500c085c000c005;
  *((unsigned long*)& __m128i_result[0]) = 0xe5c1a185c48004c5;
  __m128i_out = __lsx_vnori_b(__m128i_op0,0x1a);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
