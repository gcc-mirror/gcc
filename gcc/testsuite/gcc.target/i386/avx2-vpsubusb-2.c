/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_b u, s1, s2;
  unsigned char e[32];
  int i, tmp;

  s1.x = _mm256_set_epi8 (1, 2, 3, 4, 10, 20, 30, 90, 80, 40, 100, 15,
			  98, 25, 98, 7, 88, 44, 33, 22, 11, 98, 76,
			  200, 34, 78, 39, 6, 3, 4, 5, 119);

  s2.x = _mm256_set_epi8 (88, 44, 33, 220, 11, 98, 76, 100, 34, 78, 39,
			  6, 3, 4, 5, 219, 1, 2, 3, 4, 10, 20, 30, 90,
			  80, 40, 100, 15, 98, 25, 98, 7);

  u.x = _mm256_subs_epu8 (s1.x, s2.x);

  for (i = 0; i < 32; i++)
    {
      tmp = (unsigned char) s1.a[i] - (unsigned char) s2.a[i];

      if (tmp < 0)
	tmp = 0;

      e[i] = tmp;
    }

  if (check_union256i_b (u, e))
    abort ();
}
