/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256i_d u;
  int e [8] = {23, 67, 53, 6, 4, 6, 85, 234};

  u.x = _mm256_loadu_si256 ((__m256i *) e); 

  if (check_union256i_d (u, e))
    abort ();
}
