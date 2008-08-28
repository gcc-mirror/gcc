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

static void
__attribute__((noinline, unused))
test (double *e, __m128d a)
{
  return _mm_storel_pd (e, a); 
}

static void
TEST (void)
{
  union128d u;
  double e[2];

  u.x = _mm_set_pd (41124.234,2344.2354);

  test (e, u.x);  

  e[1] = u.a[1];
  
  if (check_union128d (u, e))
    abort ();
}
