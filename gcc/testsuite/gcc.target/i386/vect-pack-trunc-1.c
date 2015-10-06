/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -mavx512bw -save-temps" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

#define N 400
unsigned char yy[10000];

void
__attribute__ ((noinline)) foo (unsigned short s)
{
   unsigned short i;
   for (i = 0; i < s; i++)
     yy[i] = (unsigned char) i;
}

void
avx512bw_test ()
{
  unsigned short i;
  foo (N);

  for (i = 0; i < N; i++)
    if ( (unsigned char)i != yy [i] )
      abort ();
}

/* { dg-final { scan-assembler-times "vpmovwb\[ \\t\]+\[^\n\]*%zmm" 2 } } */
