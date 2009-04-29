/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-skip-if "no M_PI" { vxworks_kernel } } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

static void
TEST (void)
{
  union128d u, s;
  double e[2] = {0.0};
  int i;

  s.x = _mm_set_pd (1.1234, -2.3478);
  u.x = _mm_ceil_pd (s.x);

  for (i = 0; i < 2; i++)
    {
      __m128d tmp = _mm_load_sd (&s.a[i]);
      tmp = _mm_ceil_sd (tmp, tmp);
      _mm_store_sd (&e[i], tmp);
    }
  
  if (check_union128d (u, e))
    abort ();
} 
