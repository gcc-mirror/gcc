/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 32
#define COEF 32470
#define COEF2 324700

unsigned char in[N];
int out[N];
int out2[N];

__attribute__ ((noinline)) void
foo (int a)
{
  int i;

  for (i = 0; i < N; i++)
    {
      out[i] = in[i] * COEF;
      out2[i] = in[i] + a;
    }
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      __asm__ volatile ("");
    }

  foo (COEF2);

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (out[i] != in[i] * COEF || out2[i] != in[i] + COEF2)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 1 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 1 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */

