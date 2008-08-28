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

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  return _mm_packs_epi16 (s1, s2); 
}

static void
TEST (void)
{
  union128i_w s1, s2;
  union128i_b u;
  char e[16];
  int i;
   
  s1.x = _mm_set_epi16 (2134, -128, 1234, 6354, 1002, 3004, 4050, 9999);
  s2.x = _mm_set_epi16 (41124, 234, 2344, 2354, 607, 1, 2, -8009);
  u.x = test (s1.x, s2.x); 

  for (i = 0; i < 8; i++)
    {
      if (s1.a[i] > 127)
        e[i] = 127;
      else if (s1.a[i] < -128)
        e[i] = -128;
      else
        e[i] = s1.a[i];
    }
  
  for (i = 0; i < 8; i++)
   {
      if (s2.a[i] > 127)
        e[i+8] = 127;
      else if (s2.a[i] < -128)
        e[i+8] = -128;
      else
        e[i+8] = s2.a[i];
    }

  if (check_union128i_b (u, e))
    abort ();
}
