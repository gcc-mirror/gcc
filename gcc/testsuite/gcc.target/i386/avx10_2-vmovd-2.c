/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-require-effective-target avx10_2_256 } */

#define AVX10_2
#define AVX10_SCALAR

#include "avx10-helper.h"
#include "avx10_2-vmovd-1.c"

static void
TEST (void)
{
  union128i_d u1, s1;
  int e1[4] = {0};

  s1.x = _mm_set_epi32(-12876, -12886, -12776, 3376590);
  e1[0] = s1.a[0];

  u1.x = _mm_set_epi32(-1, -1, -1, -1);
  u1.x = (__m128i)f1((int)s1.a[0]);
  if (check_union128i_d (u1, e1))
    abort ();

  u1.x = _mm_set_epi32(-1, -1, -1, -1);
  u1.x = (__m128i)f2(((float*)s1.a)[0]);
  if (check_union128i_d (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi32(-1, -1, -1, -1);
  u1.x = (__m128i)f3((v4si)s1.x);
  if (check_union128i_d (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi32(-1, -1, -1, -1);
  u1.x = (__m128i)f4((v4sf)s1.x);
  if (check_union128i_d (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi32(-1, -1, -1, -1);
  u1.x = (__m128i)f5((__m128i)s1.x);
  if (check_union128i_d (u1, e1))
    abort ();
}
