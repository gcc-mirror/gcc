/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_packssdw_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
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
#if DEBUG
  {
      printf ("sse2_test_packssdw_1; check_union128i_w failed\n");
      printf (
	  "\t ([%x,%x,%x,%x], [%x,%x,%x,%x]) -> [%x,%x,%x,%x, %x,%x,%x,%x]\n",
	  s1.a[0], s1.a[1], s1.a[2], s1.a[3], s2.a[0], s2.a[1], s2.a[2],
	  s2.a[3], u.a[0], u.a[1], u.a[2], u.a[3], u.a[4], u.a[5], u.a[6],
	  u.a[7]);
      printf ("\t expect [%x,%x,%x,%x, %x,%x,%x,%x]\n", e[0], e[1], e[2], e[3],
			  e[4], e[5], e[6], e[7]);
  }
#else
    abort ();
#endif
}
