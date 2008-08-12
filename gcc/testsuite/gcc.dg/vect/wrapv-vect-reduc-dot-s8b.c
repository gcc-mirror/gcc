/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT -21856

signed char X[N] __attribute__ ((__aligned__(16)));
signed char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->short dot product.
   The dot-product pattern should be detected.
   Should be vectorized on vect_sdot_qi targets (targets that support 
   dot-product of signed char).  
   This test currently fails to vectorize on targets that support
   dot-product of chars into and int accumulator.
   Can also be vectorized as widening-mult + summation,
   or with type-conversion support.
 */
__attribute__ ((noinline)) short
foo(int len) {
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
  short dot;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    if (i%5 == 0)
      X[i] = i;
  }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 1 "vect" } } */

/* When vectorizer is enhanced to vectorize accumulation into short for targets 
   that support accumulation into int (e.g. ia64) we'd have:
dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_qi } }
*/
/* In the meantime expect: */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_qi_to_hi || vect_unpack } } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */
