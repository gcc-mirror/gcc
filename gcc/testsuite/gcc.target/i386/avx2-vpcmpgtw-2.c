/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
avx2_test (void)
{
  union256i_w u, s1, s2;
  short e[16];
  int i;

  s1.x = _mm256_set_epi16 (1, 2, 3, 4, 10, 20, 30, 90, -80, -40, -100,
			   76, -100, -34, -78, -31000);

  s2.x = _mm256_set_epi16 (88, 44, 3, 22, 11, 98, 76, -100, -34, -78,
			   30, 90, -80, -40, -100, -15);

  u.x = _mm256_cmpgt_epi16 (s1.x, s2.x);

  for (i = 0; i < 16; i++)
    e[i] = (s1.a[i] > s2.a[i]) ? -1 : 0;

  if (check_union256i_w (u, e))
    abort ();
}
