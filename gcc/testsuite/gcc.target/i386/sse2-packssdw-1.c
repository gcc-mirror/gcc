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
  return _mm_packs_epi32 (s1, s2); 
}

static void
TEST (void)
{
  union128i_d s1, s2;
  union128i_w u;
  short e[8];
  int i;
   
  s1.x = _mm_set_epi32 (2134, -128, 655366, 9999);
  s2.x = _mm_set_epi32 (41124, 234, 2, -800900);
  u.x = test (s1.x, s2.x); 

  for (i = 0; i < 4; i++)
    {
      if (s1.a[i] > 32767)
        e[i] = 32767;
      else if (s1.a[i] < -32768)
        e[i] = -32768;
      else
        e[i] = s1.a[i];
    }
  
  for (i = 0; i < 4; i++)
   {
      if (s2.a[i] > 32767)
        e[i+4] = 32767;
      else if (s2.a[i] < -32768)
        e[i+4] = -32768;
      else
        e[i+4] = s2.a[i];
    }

  if (check_union128i_w (u, e))
    abort ();
}
