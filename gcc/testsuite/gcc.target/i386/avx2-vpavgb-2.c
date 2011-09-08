/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_b u, s1, s2;
  unsigned char e[32];
  int tmp;
  int i;

  s1.x = _mm256_set_epi8 (1, 2, 3, 4, 10, 20, 30, 90, -80, -40, -100,
			  -15, 98, 25, 98, 7, 88, 44, 33, 22, 11, 98,
			  76, -100, -34, -78, -39, 6, 3, 4, 5, 119);

  s2.x = _mm256_set_epi8 (88, 44, 33, 22, 11, 98, 76, -100, -34, -78,
			  -39, 6, 3, 4, 5, 119, 1, 2, 3, 4, 10, 20,
			  30, 90, -80, -40, -100, -15, 98, 25, 98, 7);

  u.x = _mm256_avg_epu8 (s1.x, s2.x);

  for (i = 0; i < 32; i++)
    e[i] = ((unsigned char) s1.a[i] + (unsigned char) s2.a[i] + 1) >> 1;

  if (check_union256i_b (u, e))
    abort ();
}
