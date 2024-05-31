/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define DOT 43680

signed short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* short->int->int dot product.
   Detected as a dot-product pattern.
   Vectorized on targets that support dot-product for signed shorts.  */

__attribute__ ((noinline)) int
foo (int len)
{
  int i;
  int result = 0;

  for (i = 0; i < len; i++)
    {
      result += (X[i] * Y[i]);
    }
  return result;
}


int
main (void)
{
  int i;
  int dot;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      X[i] = i;
      Y[i] = N - i;
      __asm__ volatile ("");
    }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected(?:(?!Analysis failed).)*Analysis succeeded" 1 "vect" { target { vect_sdot_hi || vect_widen_mult_hi_to_si } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_hi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_mult_hi_to_si } } } */

