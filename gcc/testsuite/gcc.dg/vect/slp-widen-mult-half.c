/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
#include <stdlib.h>

#define N 32
#define COEF 32470
#define COEF2 324700

unsigned char in[N];
int out[N];
int out2[N];

__attribute__ ((noinline)) void
foo ()
{
  int i;

  for (i = 0; i < N/2; i++)
    {
      out[2*i] = in[2*i] * COEF;
      out2[2*i] = in[2*i] + COEF2;
      out[2*i+1] = in[2*i+1] * COEF;
      out2[2*i+1] = in[2*i+1] + COEF2;
    }
}

int main (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      __asm__ volatile ("");
    }

  foo ();

  for (i = 0; i < N; i++)
    if (out[i] != in[i] * COEF || out2[i] != in[i] + COEF2)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

