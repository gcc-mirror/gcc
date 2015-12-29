/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union128d s1;
  unsigned long long d;
  unsigned long long e;

  s1.x = _mm_set_pd (24.43, 68.346);
  d =  _mm_cvtsd_u64 (s1.x);
  e = (unsigned long long)(s1.a[0] + 0.5);

  if (e != d)
    abort ();
}
