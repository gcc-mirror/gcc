/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pmovmskb_1
#endif

#include <emmintrin.h>

#ifdef _ARCH_PWR8
static int
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_movemask_epi8 (s1); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  union128i_b s1;
  int i, u, e=0;
   
  s1.x = _mm_set_epi8 (1,2,3,4,10,20,30,90,-80,-40,-100,-15,98, 25, 98,7);

  __asm("" : "+v"(s1.x));
  u = test (s1.x); 
  
  for (i = 0; i < 16; i++)
    if (s1.a[i] & (1<<7))
      e = e | (1<<i);

  if (checkVi (&u, &e, 1))
    {
#if DEBUG
      printf ("sse2_test_pmovmskb_1; checkVi failed\n");
      printf ("\t ([%x,%x,%x,%x, %x,%x,%x,%x,"
	      " %x,%x,%x,%x, %x,%x,%x,%x], -> %x)\n",
	      s1.a[0], s1.a[1], s1.a[2], s1.a[3], s1.a[4], s1.a[5], s1.a[6],
	      s1.a[7], s1.a[8], s1.a[9], s1.a[10], s1.a[11], s1.a[12],
	      s1.a[13], s1.a[14], s1.a[15], u);
      printf ("\t expect %x\n", e);
#endif
      abort ();
    }
#endif
}
