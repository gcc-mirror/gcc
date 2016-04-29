/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -mavx512bw -save-temps" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

#define N 120
signed int yy[10000];
signed char zz[10000];

void
__attribute__ ((noinline,noclone)) foo (int s)
{
   int i;
   for (i = 0; i < s; i++)
     yy[i] = zz[i];
}

void
avx512bw_test ()
{
  signed char i;
  for (i = 0; i < N; i++)
    zz[i] = i;
  foo (N);
  for (i = 0; i < N; i++)
    if ( (signed int)i != yy [i] )
      abort ();
}

/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\n\]*%zmm" 2 } } */
