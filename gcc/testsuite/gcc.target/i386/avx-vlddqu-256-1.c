/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int e[8]={ 23, 67, 53, 6, 4, 6, 85, 234};
  union256i_d u;

  u.x = _mm256_lddqu_si256 ((__m256i *) e);

  if (check_union256i_d (u, e))
    abort ();
}
