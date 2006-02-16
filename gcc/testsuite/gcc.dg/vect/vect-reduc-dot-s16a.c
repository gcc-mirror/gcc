/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define DOT 43680

signed short X[N] __attribute__ ((__aligned__(16)));
signed short Y[N] __attribute__ ((__aligned__(16)));

/* short->int->int dot product.
   Detected as a dot-product pattern.
   Vectorized on targets that support dot-product for signed shorts.  */

int
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
    }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_hi } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

