/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256 u, s1;
  float source [8] = {2134.3343,1234.635654,453.345635,54646.464356,895833.346347,56343,234234.34563,2345434.67832};
  float e [8] = {2134.0,1234.0,453.0,54646.0,895833.0,56343,234234.0,2345434.0};

  s1.x = _mm256_loadu_ps (source);
  u.x = _mm256_round_ps (s1.x, 1);

  if (check_union256 (u, e))
    abort ();
}
