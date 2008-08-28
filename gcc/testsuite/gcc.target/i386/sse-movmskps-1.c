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

static int
__attribute__((noinline, unused))
test (__m128 a)
{
  return _mm_movemask_ps (a); 
}

static void
TEST (void)
{
  union128 u;
  float s[4] = {2134.3343,1234.635654, 1.2234, 876.8976};
  int d;
  int e = 0;
  int i;

  u.x = _mm_loadu_ps (s);   
  d = test (u.x);

  for (i = 0; i < 4; i++)
    if (s[i] < 0)
      e |= (1 << i);

  if (checkVi (&d, &e, 1))
    abort ();

}
