/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static unsigned int
__attribute__((noinline, unused))
test (union128d s1)
{
  return _mm_cvttsd_u32 (s1.x);
}

void static
avx512f_test (void)
{
  union128d s1;
  unsigned int d;
  unsigned int e;

  s1.x = _mm_set_pd (24.43, 68.346);
  d =  test (s1);
  e = (unsigned int)s1.a[0];

  if (e != d)
    abort ();
}
