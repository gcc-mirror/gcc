/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int s1i[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int s2i[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  int d;
  int c = 1, z = 1, e = 0xf;
  int i;
  union256i_d s1, s2;

  s1.x = _mm256_loadu_si256 ((__m256i*)s1i);
  s2.x = _mm256_loadu_si256 ((__m256i*)s2i);
  d = _mm256_testnzc_si256 (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    {
      if ((s1.a[i] & s2.a[i]))
        z = 0;
      if ((~s1.a[i] & s2.a[i]))
        c = 0;
    }

   e = (z == 0 && c == 0) ? 1 : 0;

  if (d != e)
    abort ();
}
