/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -fno-tree-loop-distribute-patterns -ftree-vectorize -fdump-tree-vect-details -fno-vect-cost-model" } */
/* { dg-additional-options "-mno-sse" { target { i?86-*-* x86_64-*-* } } } */
/* The IBM Z backend sets the min-vect-loop-bound param to 2 to avoid
   awkward epilogue code generation in some cases.  This line needs to
   be removed after finding an alternate way to fix this.  */
/* { dg-additional-options "--param min-vect-loop-bound=0" { target { s390*-*-* } } } */

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
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! { avr-*-* pru-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { ! { avr-*-* pru-*-* } } } } } */
