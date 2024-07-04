/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrld_1
#endif

#define N 0xf

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_srli_epi32 (s1, N); 
}

static void
TEST (void)
{
  union128i_d u, s;
  int e[4] = { 0 };
  unsigned int tmp;
  int i;
 
  s.x = _mm_set_epi32 (1, -2, 3, 4);

  u.x = test (s.x);

  if (N < 32)
    for (i = 0; i < 4; i++)
      {
        tmp  = s.a[i];
        e[i] = tmp >> N; 
      }

  if (check_union128i_d (u, e))
    {
#if DEBUG
      printf ("sse2_test_psrld_1; check_union128i_d failed\n");
      printf ("\tsrl\t([%x,%x,%x,%x],%d\n", s.a[0], s.a[1], s.a[2], s.a[3], N);
      printf ("\t ->\t [%x,%x,%x,%x]\n", u.a[0], u.a[1], u.a[2], u.a[3]);
      printf ("\texpect\t [%x,%x,%x,%x]\n", e[0], e[1], e[2], e[3]);
#endif
      abort ();
    }
}
