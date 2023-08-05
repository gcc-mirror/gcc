/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-fno-tree-vectorize -ftree-slp-vectorize -ftree-loop-if-convert" } */

#include "tree-vect.h"

#define N 128

__attribute__((noinline, noclone)) void
foo (short * __restrict__ a, int * __restrict__ b, int stride)
{
  int i;

  for (i = 0; i < N/stride; i++, a += stride, b += stride)
   {
     a[0] = b[0] ? 1 : 7;
     a[1] = b[1] ? 2 : 7;
     a[2] = b[2] ? 3 : 0;
     a[3] = b[3] ? 4 : 7;
     a[4] = b[4] ? 5 : 0;
     a[5] = b[5] ? 6 : 0;
     a[6] = b[6] ? 7 : 0;
     a[7] = b[7] ? 8 : 7;
   }
}

short a[N];
int b[N];
int main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = -i;
    }

  foo (a, b, 8);

#pragma GCC novector
  for (i = 1; i < N; i++)
    if (a[i] != i%8 + 1)
      abort ();

  if (a[0] != 7)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp1" { target { vect_element_align && vect_pack_trunc } } } } */
