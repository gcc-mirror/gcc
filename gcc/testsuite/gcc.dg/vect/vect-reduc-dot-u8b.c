/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT 43680

unsigned char X[N] __attribute__ ((__aligned__(16)));
unsigned char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->short dot product. 
   Detected as a dot-product pattern.
   Should be vectorized on targets that support dot-product for unsigned chars,
   but currently this test cannot be vectorized as a dot-product on targets
   that support char->short->int dot-product. 
   Alternatively, this test can be vectorized using vect_widen_mult_qi (or
   vect_unpack and non-widening multplication: vect_unpack && vect_short_mult).
   */
__attribute__ ((noinline)) unsigned short
foo (int len) {
  int i;
  unsigned short result = 0;

  for (i=0; i<len; i++) {
    result += (unsigned short)(X[i] * Y[i]);
  }
  return result;
}

int main (void)
{
  unsigned short dot;
  int i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    /* Avoid vectorization.  */
    if (i%100 == 0)
      X[i] = i;
  }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */

/* When the vectorizer is enhanced to vectorize accumulation into short for 
   targets that support accumulation into int (powerpc, ia64) we'd have:
dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_udot_qi || vect_widen_mult_qi_to_hi } }
*/
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" {target { vect_widen_mult_qi_to_hi || vect_unpack } } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */

