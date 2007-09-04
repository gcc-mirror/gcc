/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 43680

signed char X[N] __attribute__ ((__aligned__(16)));
signed char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->int dot product.
   The dot-product pattern should be detected.
   Vectorizable on vect_sdot_qi targets (targets that support dot-product of 
   signed chars).

   In the future could also be vectorized as widening-mult + widening-summation,
   or with type-conversion support.
 */
__attribute__ ((noinline)) int
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

int main (void)
{
  int i, dot1;

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

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_qi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_qi_to_hi && vect_widen_sum_hi_to_si } } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */
