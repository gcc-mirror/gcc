/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -fno-vect-cost-model" } */
/* { dg-additional-options "-mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 16
 
int main ()
{  
  struct {
    char ca[N];
  } s;
  int i;

  for (i = 0; i < N; i++)
    {
      s.ca[i] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! avr-*-* } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { ! avr-*-* } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
