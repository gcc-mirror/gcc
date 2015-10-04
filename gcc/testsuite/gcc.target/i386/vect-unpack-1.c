/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -mavx512bw -save-temps" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

#define N 255
unsigned int yy[10000];

void
__attribute__ ((noinline)) foo (unsigned char s)
{
   unsigned char i;
   for (i = 0; i < s; i++)
     yy[i] = (unsigned int) i;
}

void
avx512bw_test ()
{
  unsigned char i;
  foo (N);
  for (i = 0; i < N; i++)
    if ( (unsigned int)i != yy [i] )
      abort ();
}

/* { dg-final { scan-assembler-times "vpmovzxbw\[ \\t\]+\[^\n\]*%zmm" 2 } } */
