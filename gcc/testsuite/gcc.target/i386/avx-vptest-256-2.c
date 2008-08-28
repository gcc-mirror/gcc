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
  int e;
  int i;
  union256i_d s1, s2;

  s1.x = _mm256_loadu_si256 ((__m256i*)s1i);
  s2.x = _mm256_loadu_si256 ((__m256i*)s2i);
  d = _mm256_testc_si256 (s1.x, s2.x);

  e = 1;
  for (i = 0; i < 8; i++)
    if ((~s1i[i] & s2i[i]) != 0)
      e = 0;

  if (d != e)
    abort ();
}
