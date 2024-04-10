/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psadbw_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  return _mm_sad_epu8 (s1, s2); 
}

static void
TEST (void)
{
  union128i_ub s1, s2;
  union128i_w u;
  short e[8] = { 0 };
  unsigned char tmp[16];
  int i;
   
  s1.x = _mm_set_epi8 (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
  s2.x = _mm_set_epi8 (16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1);
  u.x = test (s1.x, s2.x);

  for (i = 0; i < 16; i++)
    tmp [i] = __builtin_abs (s1.a[i] - s2.a[i]);

  for (i = 0; i < 8; i++)
    e[0] += tmp[i];

  for (i = 8; i < 16; i++)
    e[4] += tmp[i]; 


  if (check_union128i_w (u, e))
    {
#if DEBUG
      printf ("sse2_test_psadbw_1; check_union128i_w failed\n");
      printf ("\tadds\t([%x,%x,%x,%x, %x,%x,%x,%x,"
	      " %x,%x,%x,%x, %x,%x,%x,%x],\n",
	      s1.a[0], s1.a[1], s1.a[2], s1.a[3], s1.a[4], s1.a[5], s1.a[6],
	      s1.a[7], s1.a[8], s1.a[9], s1.a[10], s1.a[11], s1.a[12],
	      s1.a[13], s1.a[14], s1.a[15]);
      printf ("\t\t [%x,%x,%x,%x, %x,%x,%x,%x, %x,%x,%x,%x, %x,%x,%x,%x])\n",
	      s2.a[0], s2.a[1], s2.a[2], s2.a[3], s2.a[4], s2.a[5], s2.a[6],
	      s2.a[7], s2.a[8], s2.a[9], s2.a[10], s2.a[11], s2.a[12],
	      s2.a[13], s2.a[14], s2.a[15]);
      printf ("\t ->\t [%x,%x,%x,%x, %x,%x,%x,%x]\n", u.a[0], u.a[1], u.a[2],
	      u.a[3], u.a[4], u.a[5], u.a[6], u.a[7]);
      printf ("\texpect\t [%x,%x,%x,%x, %x,%x,%x,%x]\n", e[0], e[1], e[2],
	      e[3], e[4], e[5], e[6], e[7]);
#endif
      abort ();
    }
}
