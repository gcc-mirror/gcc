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
test (__m128i s1)
{
  return _mm_movemask_epi8 (s1); 
}

static void
TEST (void)
{
  union128i_b s1;
  int i, u, e=0;
   
  s1.x = _mm_set_epi8 (1,2,3,4,10,20,30,90,-80,-40,-100,-15,98, 25, 98,7);
  u = test (s1.x); 
  
  for (i = 0; i < 16; i++)
    if (s1.a[i] & (1<<7))
      e = e | (1<<i);

  if (checkVi (&u, &e, 1))
    abort ();
}
