/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pshufhw_1
#endif

#define N 0xec

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_shufflehi_epi16 (s1, N); 
}

static void
TEST (void)
{
  union128i_w s1, u;
  short  e[8] = { 0 };
  int i;
  int m1[4] = { 0x3, 0x3<<2, 0x3<<4, 0x3<<6 };
  int m2[4];
  
  s1.x = _mm_set_epi16 (0, 0, 0xa, 0xbcde, 0, 0, 0xef58, 0xa234);
  u.x = test (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[i];

  for (i = 0; i < 4; i++) {
    int i2 = i;
#ifdef __LITTLE_ENDIAN__
    i2 = 3 - i;
#endif
    m2[i2] = (N & m1[i2]) >> (2 * i2);
  }

  for (i = 0; i < 4; i++)
    e[i + 4] = s1.a[m2[i] + 4];

  if (check_union128i_w(u, e))
    {
#if DEBUG
      union128i_w s;
      s.x = s1.x;
      printf ("sse2_test_pshufhw_1; check_union128i_w failed\n");
      printf ("\t ([%hx,%hx,%hx,%hx, %hx,%hx,%hx,%hx])\n", s.a[0], s.a[1],
	      s.a[2], s.a[3], s.a[4], s.a[5], s.a[6], s.a[7]);
      printf ("\t\t -> [%hx,%hx,%hx,%hx, %hx,%hx,%hx,%hx]\n", u.a[0], u.a[1],
	      u.a[2], u.a[3], u.a[4], u.a[5], u.a[6], u.a[7]);
      printf ("\t expect [%hx,%hx,%hx,%hx, %hx,%hx,%hx,%hx]\n", e[0], e[1],
	      e[2], e[3], e[4], e[5], e[6], e[7]);
#endif
      abort ();
    }
}
