/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT 43680

unsigned char X[N] __attribute__ ((__aligned__(16)));
unsigned char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->int dot product. 
   Detected as a dot-product pattern.
   Should be vectorized on targets that support dot-product for unsigned chars
   (vect_udot_qi),
   and on targets that support widening-multiplication and widening-summation
   (vect_widen_mult_qi && vec_widen_sum_qi_to_si).
   Widening-multiplication can also be supported by type promotion and non-widening 
   multiplication (vect_unpack && vect_short_mult);
   Widening summation can also be supported by type promotion and non-widening 
   summation (vect_unpack).
   */
__attribute__ ((noinline)) unsigned int
foo (int len) {
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
  unsigned int dot;
  int i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
  }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_udot_qi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_qi_to_hi && vect_widen_sum_qi_to_si } } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */

