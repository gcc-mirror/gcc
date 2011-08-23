/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_b u, s1, s2;
  char e[32];
  unsigned i;

  s1.x = _mm256_set_epi8 (10, 74, 50, 4, 6, 99, 1, 4, 87, 83, 84,
			  29, 81, 79, 1, 3, 1, 5, 2, 47, 20, 2, 72,
			  92, 9, 4, 23, 17, 99, 43, 72, 17);

  s2.x = _mm256_set_epi8 (88, 44, 33, 20, 56, 99, 2, 90, 38, 4, 200,
			  17, 3, 39, 2, 37, 27, 95, 17, 74, 72, 43,
			  27, 112, 71, 50, 32, 72, 84, 17, 27, 96);

  u.x = _mm256_add_epi8 (s1.x, s2.x);

  for (i = 0; i < 32; i++)
    e[i] = s1.a[i] + s2.a[i];

  if (check_union256i_b (u, e))
    abort ();
}
