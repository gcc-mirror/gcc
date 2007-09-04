/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 43680
#define DOT2 43680

unsigned short X[N] __attribute__ ((__aligned__(16)));
unsigned short Y[N] __attribute__ ((__aligned__(16)));

/* short->short->int dot product. 
   Not detected as a dot-product pattern.
   Requires support for non-widneing multiplication and widening-summation.  */
__attribute__ ((noinline)) unsigned int
foo1(int len) {
  int i;
  unsigned int result = 0;
  unsigned short prod;

  for (i=0; i<len; i++) {
    prod = X[i] * Y[i];
    result += prod;
  }
  return result;
}

int main (void)
{
  unsigned int dot1;
  unsigned short i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
  }

  dot1 = foo1 (N);
  if (dot1 != DOT1)
    abort ();

  return 0;
}

/* The initialization loop in main also gets vectorized.  */
/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_short_mult && vect_widen_sum_hi_to_si } } } } */ 
/* { dg-final { cleanup-tree-dump "vect" } } */
