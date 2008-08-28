/* { dg-do run } */
/* { dg-options "-O2 -msse" } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 a, __m64 *p)
{
  return _mm_loadh_pi (a, p); 
}

static void
TEST (void)
{
  union128 u, s1;
  float d[2] = {24.43, 68.346};
  float e[4] = {1.17, 2.16, 3.15, 4.14};

  s1.x = _mm_set_ps (5.13, 6.12, 7.11, 8.9);
  u.x = _mm_loadu_ps (e);
 
  u.x = test (s1.x, (__m64 *)d);

  e[0] = s1.a[0];
  e[1] = s1.a[1];
  e[2] = d[0];
  e[3] = d[1];

  if (check_union128 (u, e))
    abort ();
}
