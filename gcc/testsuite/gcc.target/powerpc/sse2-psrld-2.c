/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrld_2
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i c)
{
  return _mm_srl_epi32 (s1, c); 
}

static void
TEST (void)
{
  union128i_d u, s;
  union128i_q c;
  int e[4] = { 0 };
  unsigned int tmp;
  int i;
 
  s.x = _mm_set_epi32 (2, -3, 0x7000, 0x9000);
  c.x = _mm_set_epi64x (12, 23);

  __asm("" : "+v"(s.x), "+v"(c.x));
  u.x = test (s.x, c.x);

  if (c.a[0] < 32)
    for (i = 0; i < 4; i++)
      {
        tmp = s.a[i];
        e[i] = tmp >> c.a[0];
      } 

  if (check_union128i_d (u, e))
    {
#if DEBUG
      printf ("sse2_test_psrld_2; check_union128i_d failed\n");
      printf ("\tsrld\t([%x,%x,%x,%x], [%llx,%llx]\n", s.a[0], s.a[1], s.a[2],
	      s.a[3], c.a[0], c.a[1]);
      printf ("\t ->\t [%x,%x,%x,%x]\n", u.a[0], u.a[1], u.a[2], u.a[3]);
      printf ("\texpect\t [%x,%x,%x,%x]\n", e[0], e[1], e[2], e[3]);
#endif
      abort ();
    }
}
