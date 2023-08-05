/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fno-ipa-icf" } */

#include "tree-vect.h"

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

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      __asm__ volatile ("");
    }

  foo ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (out[i] != in[i] * COEF)
      abort ();

  bar ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (out[i] != in[i] * COEF)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */
/* { dg-final { scan-tree-dump-times "pattern recognized" 2 "vect" { target vect_widen_mult_hi_to_si_pattern } } } */

