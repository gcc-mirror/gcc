/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>


static int 
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtsd_si32 (p); 
}

static void
TEST (void)
{
  union128d s;
  int e;
  int d;

  s.x = _mm_set_pd (123.321, 456.987);

  d = test (s.x);

  e = (int)(s.a[0] + 0.5);

  if (d != e)
    abort ();
}
