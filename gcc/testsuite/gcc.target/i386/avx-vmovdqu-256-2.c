/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256i_d u;
  int e [8] = {0};

  u.x = _mm256_set_epi32(23, 67, 53, 6, 4, 6, 85, 234);
  _mm256_storeu_si256 ((__m256i *)e, u.x);

  if (check_union256i_d (u, e))
    abort ();
}
