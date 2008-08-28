/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 u, s1;
  float e[8];

  s1.x = _mm256_set_ps (134.3, 1234.54, 45.335, 646.456, 43.54, 473.34, 78, 89.54);
  u.x = _mm256_movehdup_ps (s1.x);

  for (i = 0; i < 4; i++)
    e[2*i] = e[2*i+1] = s1.a[2*i+1];

  if (check_union256 (u, e))
    abort ();
}
