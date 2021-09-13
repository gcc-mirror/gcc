/* PR target/99563 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -mavx -mno-vzeroupper" } */

#include "avx-check.h"
#include <immintrin.h>


__attribute__((noipa)) float
compute_generic (void)
{
  return 0.0f;
}

static inline __attribute__((always_inline))
float compute_avx (unsigned long block_count)
{
  __m128d mm_res = _mm_set1_pd (256.0);
  float res = (float) (_mm_cvtsd_f64 (mm_res) / (double) block_count);
  _mm256_zeroupper ();
  return res;
}

__attribute__((noipa)) float
compute (unsigned long block_count)
{
  if (block_count >= 64)
    return compute_avx (block_count);
  else
    return compute_generic ();
}

static void
avx_test (void)
{
  if (compute (128) != 2.0f || compute (32) != 0.0f)
    abort ();
}
