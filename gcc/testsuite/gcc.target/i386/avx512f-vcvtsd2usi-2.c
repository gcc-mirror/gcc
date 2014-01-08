/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union128d s1;
  unsigned int d;
  unsigned int e;

  s1.x = _mm_set_pd (24.43, 68.346);
  d =  _mm_cvtsd_u32 (s1.x);
  e = (unsigned int)(s1.a[0] + 0.5);

  if (e != d)
    abort ();
}
