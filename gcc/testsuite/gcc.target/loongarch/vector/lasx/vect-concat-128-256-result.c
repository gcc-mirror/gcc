/* { dg-options "-mabi=lp64d -O2 -mlasx -w -fno-strict-aliasing" } */

#include "../simd_correctness_check.h"
#include <lasxintrin.h>

int
main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  __m256i __m256i_op0, __m256i_op1, __m256i_op2, __m256i_out, __m256i_result;
  __m256 __m256_op0, __m256_op1, __m256_op2, __m256_out, __m256_result;
  __m256d __m256d_op0, __m256d_op1, __m256d_op2, __m256d_out, __m256d_result;

  //__m128_op0={1,2,3,4},__m128_op1={5,6,7,8};
  *((int *)&__m128_op0[3]) = 0x40800000;
  *((int *)&__m128_op0[2]) = 0x40400000;
  *((int *)&__m128_op0[1]) = 0x40000000;
  *((int *)&__m128_op0[0]) = 0x3f800000;
  *((int *)&__m128_op1[3]) = 0x41000000;
  *((int *)&__m128_op1[2]) = 0x40e00000;
  *((int *)&__m128_op1[1]) = 0x40c00000;
  *((int *)&__m128_op1[0]) = 0x40a00000;
  *((int *)&__m256_result[7]) = 0x41000000;
  *((int *)&__m256_result[6]) = 0x40e00000;
  *((int *)&__m256_result[5]) = 0x40c00000;
  *((int *)&__m256_result[4]) = 0x40a00000;
  *((int *)&__m256_result[3]) = 0x40800000;
  *((int *)&__m256_result[2]) = 0x40400000;
  *((int *)&__m256_result[1]) = 0x40000000;
  *((int *)&__m256_result[0]) = 0x3f800000;
  __m256_out = __lasx_concat_128_s (__m128_op0, __m128_op1);
  ASSERTEQ_32 (__LINE__, __m256_result, __m256_out);
  __m256_out = __lasx_cast_128_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m256_out, __m128_op0);

  //__m128i_op0={1,2},__m128i_op1={3,4};
  *((unsigned long *)&__m128i_op0[1]) = 0x2;
  *((unsigned long *)&__m128i_op0[0]) = 0x1;
  *((unsigned long *)&__m128i_op1[1]) = 0x4;
  *((unsigned long *)&__m128i_op1[0]) = 0x3;
  *((unsigned long *)&__m256i_result[3]) = 0x4;
  *((unsigned long *)&__m256i_result[2]) = 0x3;
  *((unsigned long *)&__m256i_result[1]) = 0x2;
  *((unsigned long *)&__m256i_result[0]) = 0x1;
  __m256i_out = __lasx_concat_128 (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);
  __m256i_out = __lasx_cast_128 (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_out, __m128i_op0);

  //__m128d_op0={1,2},__m128i_op1={3,4};
  *((unsigned long *)&__m128d_op0[1]) = 0x4000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x4010000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x4008000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x4010000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x4008000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x4000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x3ff0000000000000;
  __m256d_out = __lasx_concat_128_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);
  __m256d_out = __lasx_cast_128_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_out, __m128d_op0);

  return 0;
}
