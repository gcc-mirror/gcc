/* { dg-do run { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static unsigned long long
__attribute__((noinline, unused))
test (union128d s1)
{
  return _mm_cvttsd_u64 (s1.x);
}

void static
avx512f_test (void)
{
  union128d s1;
  unsigned long long d;
  unsigned long long e;

  s1.x = _mm_set_pd (24.43, 68.346);
  d =  test (s1);
  e = (unsigned long long)s1.a[0];

  if (e != d)
    abort ();
}
