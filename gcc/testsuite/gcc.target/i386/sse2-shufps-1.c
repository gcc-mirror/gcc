/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#define MASK 0xab

#include CHECK_H

#include <emmintrin.h>

float select4(const float *src, unsigned int control)
{
    switch(control) {
    case 0:
        return src[0];
    case 1:
        return src[1];
    case 2:
        return src[2];
    case 3:
        return src[3];
    }
    return -1;
}

static __m128
__attribute__((noinline, unused))
test (__m128 s1, __m128 s2)
{
  return _mm_shuffle_ps (s1, s2, MASK); 
}

static void
TEST (void)
{
  union128 u, s1, s2;
  float e[4] = {0.0};
   
  s1.x = _mm_set_ps (1.1, 1.2, 1.3, 1.4);
  s2.x = _mm_set_ps (2.1, 2.2, 2.3, 2.4);
  u.x = test (s1.x, s2.x);

  e[0] = select4(s1.a,   (MASK >> 0) & 0x3);
  e[1] = select4(s1.a,   (MASK >> 2) & 0x3);
  e[2] = select4(s2.a,   (MASK >> 4) & 0x3);
  e[3] = select4(s2.a,   (MASK >> 6) & 0x3);

  if (check_union128(u, e))
    abort ();
}
