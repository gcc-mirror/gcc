/* { dg-options "-mabi=lp64d -O2 -mlasx -w -fno-strict-aliasing" } */

#include "../simd_correctness_check.h"
#include <lasxintrin.h>

extern void abort (void);
int
main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_out;
  __m128 __m128_op0, __m128_op1, __m128_out;
  __m128d __m128d_op0, __m128d_op1, __m128d_out;

  __m256i __m256i_op0, __m256i_result0, __m256i_result1, __m256i_out;
  __m256 __m256_op0, __m256_result0, __m256_result1, __m256_out;
  __m256d __m256d_op0, __m256d_result0, __m256d_result1, __m256d_out;

  //__m256_op0 = {1,2,3,4,5,6,7,8}, __m128_op0 ={9,9,9,9};
  *((int *)&__m256_op0[7]) = 0x41000000;
  *((int *)&__m256_op0[6]) = 0x40e00000;
  *((int *)&__m256_op0[5]) = 0x40c00000;
  *((int *)&__m256_op0[4]) = 0x40a00000;
  *((int *)&__m256_op0[3]) = 0x40800000;
  *((int *)&__m256_op0[2]) = 0x40400000;
  *((int *)&__m256_op0[1]) = 0x40000000;
  *((int *)&__m256_op0[0]) = 0x3f800000;
  *((int *)&__m128_op0[3]) = 0x41100000;
  *((int *)&__m128_op0[2]) = 0x41100000;
  *((int *)&__m128_op0[1]) = 0x41100000;
  *((int *)&__m128_op0[0]) = 0x41100000;
  *((int *)&__m256_result0[7]) = 0x41000000;
  *((int *)&__m256_result0[6]) = 0x40e00000;
  *((int *)&__m256_result0[5]) = 0x40c00000;
  *((int *)&__m256_result0[4]) = 0x40a00000;
  *((int *)&__m256_result0[3]) = 0x41100000;
  *((int *)&__m256_result0[2]) = 0x41100000;
  *((int *)&__m256_result0[1]) = 0x41100000;
  *((int *)&__m256_result0[0]) = 0x41100000;
  *((int *)&__m256_result1[7]) = 0x41100000;
  *((int *)&__m256_result1[6]) = 0x41100000;
  *((int *)&__m256_result1[5]) = 0x41100000;
  *((int *)&__m256_result1[4]) = 0x41100000;
  *((int *)&__m256_result1[3]) = 0x40800000;
  *((int *)&__m256_result1[2]) = 0x40400000;
  *((int *)&__m256_result1[1]) = 0x40000000;
  *((int *)&__m256_result1[0]) = 0x3f800000;
  __m256_out = __lasx_insert_128_lo_s (__m256_op0, __m128_op0);
  ASSERTEQ_32 (__LINE__, __m256_result0, __m256_out);
  __m256_out = __lasx_insert_128_hi_s (__m256_op0, __m128_op0);
  ASSERTEQ_32 (__LINE__, __m256_result1, __m256_out);

  //__m256i_op0 ={1,2,3,4},__m128i_op0={5,6},__m128i_op1={7,8};
  *((unsigned long *)&__m256i_op0[3]) = 0x4;
  *((unsigned long *)&__m256i_op0[2]) = 0x3;
  *((unsigned long *)&__m256i_op0[1]) = 0x2;
  *((unsigned long *)&__m256i_op0[0]) = 0x1;
  *((unsigned long *)&__m128i_op0[1]) = 0x6;
  *((unsigned long *)&__m128i_op0[0]) = 0x5;
  *((unsigned long *)&__m128i_op1[1]) = 0x8;
  *((unsigned long *)&__m128i_op1[0]) = 0x7;
  *((unsigned long *)&__m256i_result0[3]) = 0x4;
  *((unsigned long *)&__m256i_result0[2]) = 0x3;
  *((unsigned long *)&__m256i_result0[1]) = 0x6;
  *((unsigned long *)&__m256i_result0[0]) = 0x5;
  *((unsigned long *)&__m256i_result1[3]) = 0x8;
  *((unsigned long *)&__m256i_result1[2]) = 0x7;
  *((unsigned long *)&__m256i_result1[1]) = 0x2;
  *((unsigned long *)&__m256i_result1[0]) = 0x1;
  __m256i_out = __lasx_insert_128_lo (__m256i_op0, __m128i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result0, __m256i_out);
  __m256i_out = __lasx_insert_128_hi (__m256i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result1, __m256i_out);

  //__m256d_op0 ={1,2,3,4},__m128d_op0={5,6},__m128d_op1={7,8};
  *((unsigned long *)&__m256d_op0[3]) = 0x4010000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x4008000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x4000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128d_op0[1]) = 0x4018000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x4014000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x4020000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x401c000000000000;
  *((unsigned long *)&__m256d_result0[3]) = 0x4010000000000000;
  *((unsigned long *)&__m256d_result0[2]) = 0x4008000000000000;
  *((unsigned long *)&__m256d_result0[1]) = 0x4018000000000000;
  *((unsigned long *)&__m256d_result0[0]) = 0x4014000000000000;
  *((unsigned long *)&__m256d_result1[3]) = 0x4020000000000000;
  *((unsigned long *)&__m256d_result1[2]) = 0x401c000000000000;
  *((unsigned long *)&__m256d_result1[1]) = 0x4000000000000000;
  *((unsigned long *)&__m256d_result1[0]) = 0x3ff0000000000000;
  __m256d_out = __lasx_insert_128_lo_d (__m256d_op0, __m128d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result0, __m256d_out);
  __m256d_out = __lasx_insert_128_hi_d (__m256d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m256d_result1, __m256d_out);

  return 0;
}
