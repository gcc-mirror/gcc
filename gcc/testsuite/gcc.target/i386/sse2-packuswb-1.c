/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

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
  return _mm_packus_epi16 (s1, s2); 
}

static void
TEST (void)
{
  union128i_w s1, s2;
  union128i_ub u;
  unsigned char e[16];
  int i, tmp;
   
  s1.x = _mm_set_epi16 (1, 2, 3, 4, -5, -6, -7, -8);
  s2.x = _mm_set_epi16 (-9, -10, -11, -12, 13, 14, 15, 16);
  u.x = test (s1.x, s2.x); 

  for (i=0; i<8; i++)
    {
      tmp = s1.a[i]<0 ? 0 : s1.a[i];
      tmp = tmp>255 ? 255 : tmp;
      e[i] = tmp;

      tmp = s2.a[i]<0 ? 0 : s2.a[i];
      tmp = tmp>255 ? 255 : tmp;
      e[i+8] = tmp;
    }

  if (check_union128i_ub (u, e))
    abort ();
}
