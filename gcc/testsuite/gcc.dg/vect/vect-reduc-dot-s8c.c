/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT3 43680

signed char X[N] __attribute__ ((__aligned__(16)));
signed char Y[N] __attribute__ ((__aligned__(16)));

/* char->int->int dot product. 
   Not detected as a dot-product pattern.
   Currently fails to be vectorized due to presence of type conversions. */
__attribute__ ((noinline)) int
foo3(int len) {
  int i;
  int result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}

int main (void)
{
  int i, dot3;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
  }

  dot3 = foo3 (N);
  if (dot3 != DOT3)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */
