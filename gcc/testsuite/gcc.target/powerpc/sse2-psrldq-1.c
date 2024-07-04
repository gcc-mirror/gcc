/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrldq_1
#endif

#define N 0x5

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_srli_si128 (s1, N); 
}

static void
TEST (void)
{
  union128i_b u, s;
  char src[16] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };
  char e[16] = { 0 };
  int i;
   
  s.x = _mm_loadu_si128 ((__m128i *)src);

  u.x = test (s.x);

  for (i = 0; i < 16-N; i++)
    e[i] = src[i+N];

  if (check_union128i_b (u, e))
    {
#if DEBUG
      printf ("sse2_test_psrldq_1; check_union128i_b failed\n");
      printf ("\tsrl\t([%x,%x,%x,%x, %x,%x,%x,%x,"
	      " %x,%x,%x,%x, %x,%x,%x,%x],\n",
	      s.a[0], s.a[1], s.a[2], s.a[3], s.a[4], s.a[5], s.a[6], s.a[7],
	      s.a[8], s.a[9], s.a[10], s.a[11], s.a[12], s.a[13], s.a[14],
	      s.a[15]);
      printf ("\t ->\t [%x,%x,%x,%x, %x,%x,%x,%x, %x,%x,%x,%x, %x,%x,%x,%x]\n",
	      u.a[0], u.a[1], u.a[2], u.a[3], u.a[4], u.a[5], u.a[6], u.a[7],
	      u.a[8], u.a[9], u.a[10], u.a[11], u.a[12], u.a[13], u.a[14],
	      u.a[15]);
      printf ("\texpect\t [%x,%x,%x,%x, %x,%x,%x,%x,"
	      " %x,%x,%x,%x, %x,%x,%x,%x]\n",
	      e[0], e[1], e[2], e[3], e[4], e[5], e[6], e[7], e[8], e[9],
	      e[10], e[11], e[12], e[13], e[14], e[15]);
#endif
      abort ();
    }
}
