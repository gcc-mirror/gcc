/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
#include <stdlib.h>

#define N 32
#define COEF 32470

unsigned char in[N];
int out[N];

__attribute__ ((noinline)) void
foo ()
{
  int i;

  for (i = 0; i < N; i++)
    out[i] = in[i] * COEF;
}

__attribute__ ((noinline)) void
bar ()
{
  int i;

  for (i = 0; i < N; i++)
    out[i] = COEF * in[i];
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
    if (out[i] != in[i] * COEF)
      abort ();

  bar ();

  for (i = 0; i < N; i++)
    if (out[i] != in[i] * COEF)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

