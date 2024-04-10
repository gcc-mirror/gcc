/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT2 -21856

signed char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* char->short->short dot product.
   The dot-product pattern should be detected.

   When the dot-product is detected, the loop should be vectorized on vect_sdot_qi 
   targets (targets that support dot-product of signed char).  
   This test would currently fail to vectorize on targets that support
   dot-product of chars into an int accumulator.
   Alternatively, the loop could also be vectorized as widening-mult + summation,
   or with type-conversion support.
 */
__attribute__ ((noinline)) short
foo2(int len) {
  int i;
  short result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}

int main (void)
{
  int i;
  short dot2;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    __asm__ volatile ("");
  }

  dot2 = foo2 (N);
  if (dot2 != DOT2)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected(?:(?!failed)(?!Re-trying).)*succeeded" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected(?:(?!failed)(?!Re-trying).)*succeeded" 1 "vect" { target vect_widen_mult_qi_to_hi } } } */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_mult_qi_to_hi } } } */

