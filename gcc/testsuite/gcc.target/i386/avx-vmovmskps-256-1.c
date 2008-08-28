/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  int d;
  union256 s1;
  float source[8] = {-45, -3, -34.56, 35, 5.46,46, -464.3, 56};
  int e = 0;

  s1.x = _mm256_loadu_ps (source);
  d = _mm256_movemask_ps (s1.x);
  
  for (i = 0; i < 8; i++)
    if (source[i] < 0)
      e |= (1 << i);

  if (checkVi (&d, &e, 1))
    abort ();  
}
