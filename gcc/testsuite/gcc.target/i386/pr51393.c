/* { dg-do run { target { ! { ia32 } } } } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O -mavx" } */

#include "avx-check.h"
#include <immintrin.h>

static void
__attribute__((noinline))
avx_test (void)
{
  long long in = 0x800000000ll;
  long long out;

  __m256i zero = _mm256_setzero_si256();
  __m256i tmp  = _mm256_insert_epi64 (zero, in, 0);
  out = _mm256_extract_epi64(tmp, 0);

  if (in != out)
    abort ();
}
