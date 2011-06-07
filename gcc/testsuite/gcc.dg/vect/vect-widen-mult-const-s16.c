/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
#include <stdlib.h>

#define N 32

__attribute__ ((noinline)) void 
foo (int *__restrict a,
     short *__restrict b,
     int n)
{
  int i;

  for (i = 0; i < n; i++)
    a[i] = b[i] * 2333;

  for (i = 0; i < n; i++)
    if (a[i] != b[i] * 2333)
      abort ();
}

__attribute__ ((noinline)) void
bar (int *__restrict a,
     short *__restrict b,
     int n)
{
  int i;

  for (i = 0; i < n; i++)
    a[i] = b[i] * (short) 2333;

  for (i = 0; i < n; i++)
    if (a[i] != b[i] * (short) 2333)
      abort ();
}

int main (void)
{
  int i;
  int a[N];
  short b[N];

  for (i = 0; i < N; i++)
    {
      a[i] = 0;
      b[i] = i;
      __asm__ volatile ("");
    }

  foo (a, b, N);
  bar (a, b, N);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

