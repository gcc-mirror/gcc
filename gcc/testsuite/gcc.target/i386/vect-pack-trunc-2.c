/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -mavx512bw -save-temps" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

#define N 400
unsigned short yy[10000];

void
__attribute__ ((noinline)) foo (unsigned int s)
{
   unsigned int i;
   for (i = 0; i < s; i++)
     yy[i] = (unsigned short) i;
}

void
avx512bw_test ()
{
  unsigned int i;
  foo (N);
#pragma GCC novector
  for (i = 0; i < N; i++)
    if ( (unsigned short)i != yy [i] )
      abort ();
}

/* { dg-final { scan-assembler-times "vperm\[it]2w\[ \\t\]+\[^\n\]*%zmm" 1 } } */
