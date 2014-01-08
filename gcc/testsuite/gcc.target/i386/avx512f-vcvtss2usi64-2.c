/* { dg-do run { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union128 s1;
  unsigned long long d;
  unsigned long long e;

  s1.x = _mm_set_ps (24.43, 68.346, 12.34, 80.67);
  d =  _mm_cvtss_u64 (s1.x);
  e = (unsigned long long)(s1.a[0] + 0.5);

  if (e != d)
    abort ();
}
