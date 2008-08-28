/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  int d;
  union256d s1;
  double source[4] = {-45, -3, -34.56, 35};
  int e = 0;

  s1.x = _mm256_loadu_pd (source);
  d = _mm256_movemask_pd (s1.x);
  
  for (i = 0; i < 4; i++)
    if (source[i] < 0)
      e |= (1 << i);

  if (checkVi (&d, &e, 1))
    abort ();  
}
