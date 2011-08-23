/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_w u, s1, s2;
  short e[16];
  int i, tmp;

  s1.x = _mm256_set_epi16 (1, 2, 3, 4, 10, 20, 30, 90, -80,
			   -40, -100, -15, 98, 25, 98, 7);

  s2.x = _mm256_set_epi16 (88, 44, 33, 22, 11, 98, 76, -100,
			   -34, -78, -39, 6, 3, 4, 5, 119);

  u.x = _mm256_subs_epi16 (s1.x, s2.x);

  for (i = 0; i < 16; i++)
    {
      tmp = s1.a[i] - s2.a[i];

      if (tmp > 32767)
	tmp = 32767;
      if (tmp < -32768)
	tmp = -32768;

      e[i] = tmp;
    }

  if (check_union256i_w (u, e))
    abort ();
}
