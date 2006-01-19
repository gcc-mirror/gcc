/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 43680
#define DOT2 43680

signed short X[N] __attribute__ ((__aligned__(16)));
signed short Y[N] __attribute__ ((__aligned__(16)));

/* short->short->int dot product. 
   Not detected as a dot-product pattern.
   Currently fails to be vectorized due to presence of type conversions. */
int
foo1(int len) {
  int i;
  int result = 0;
  short prod;

  for (i=0; i<len; i++) {
    prod = X[i] * Y[i];
    result += prod;
  }
  return result;
}

/* short->int->int dot product.
   Detected as a dot-product pattern.
   Vectorized on targets that support dot-product for signed shorts.  */ 
int
foo2(int len) {
  int i;
  int result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}


int main (void)
{
  int i, dot1, dot2;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
  }

  dot1 = foo1 (N);
  if (dot1 != DOT1)
    abort ();

  dot2 = foo2 (N);
  if (dot2 != DOT2)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_hi } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

