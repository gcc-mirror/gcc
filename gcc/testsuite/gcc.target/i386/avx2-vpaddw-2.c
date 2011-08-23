/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_w u, s1, s2;
  short e[16];
  unsigned i;

  s1.x = _mm256_set_epi16 (100, 74, 50000, 4, 6999, 39999, 1000, 4,
			   874, 2783, 29884, 2904, 2889, 3279, 1, 3);
  s2.x = _mm256_set_epi16 (88, 44, 33, 220, 4556, 2999, 2, 9000,
			   238, 194, 274, 17, 3, 5739, 2, 379);

  u.x = _mm256_add_epi16 (s1.x, s2.x);

  for (i = 0; i < 16; i++)
    e[i] = s1.a[i] + s2.a[i];

  if (check_union256i_w (u, e))
    abort ();
}
