/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#include "sse2-check.h"

#ifndef TEST
#define TEST sse2_test_mmx_1
#endif

#include <mmintrin.h>

#define N 4

unsigned long long a[N], b[N], result[N];

unsigned long long check_data[N] =
  { 0x101010101010100full,
    0x1010101010101010ull,
    0x1010101010101010ull,
    0x1010101010101010ull };

__m64
unsigned_add3 (const __m64 * a, const __m64 * b,
	       __m64 * result, unsigned int count)
{
  __m64 _a, _b, one, sum, carry, onesCarry;

  unsigned int i;

  carry = _mm_setzero_si64 ();

  one = _mm_cmpeq_pi8 (carry, carry);
  one = _mm_sub_si64 (carry, one);

  for (i = 0; i < count; i++)
    {
      _a = a[i];
      _b = b[i];

      sum = _mm_add_si64 (_a, _b);
      sum = _mm_add_si64 (sum, carry);

      result[i] = sum;

      onesCarry = _mm_and_si64 (_mm_xor_si64 (_a, _b), carry);
      onesCarry = _mm_or_si64 (_mm_and_si64 (_a, _b), onesCarry);
      onesCarry = _mm_and_si64 (onesCarry, one);

      _a = _mm_srli_si64 (_a, 1);
      _b = _mm_srli_si64 (_b, 1);

      carry = _mm_add_si64 (_mm_add_si64 (_a, _b), onesCarry);
      carry = _mm_srli_si64 (carry, 63);
    }

  return carry;
}

void __attribute__((noinline))
TEST (void)
{
  unsigned long long carry;
  int i;

  /* Really long numbers.  */
  a[3] = a[2] = a[1] = a[0] = 0xd3d3d3d3d3d3d3d3ull;
  b[3] = b[2] = b[1] = b[0] = 0x3c3c3c3c3c3c3c3cull;

  carry = (unsigned long long) unsigned_add3
    ((__m64 *)a, (__m64 *)b, (__m64 *)result, N);

  _mm_empty ();

  if (carry != 1)
    abort ();

  for (i = 0; i < N; i++)
    if (result [i] != check_data[i])
      abort ();
}
