/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -fno-tree-loop-distribute-patterns -ftree-vectorize -fdump-tree-vect-details -fvect-cost-model=dynamic" } */
/* { dg-additional-options "-mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 128

/* unaligned store.  */

int main ()
{
  int i;
  char ia[N+1];

  for (i = 1; i <= N; i++)
    {
      ia[i] = 5;
    }

  /* check results:  */
  #pragma GCC novector
  for (i = 1; i <= N; i++)
    {
      if (ia[i] != 5)
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! { avr-*-* pru-*-* } } } } } */
/* IBM Z does not require special alignment for vectorization.  */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { ! { avr-*-* pru-*-* s390*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target { ! { avr-*-* pru-*-* s390*-*-* } } } } } */
