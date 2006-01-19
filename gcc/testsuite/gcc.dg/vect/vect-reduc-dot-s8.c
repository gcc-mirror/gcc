/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 43680
#define DOT2 -21856
#define DOT3 43680

signed char X[N] __attribute__ ((__aligned__(16)));
signed char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->int dot product.
   The dot-product pattern should be detected.
   Vectorizable on vect_sdot_qi targets (targets that support dot-product of 
   signed chars).

   In the future could also be vectorized as widening-mult + widening-summation,
   or with type-conversion support.
 */
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

/* char->short->short dot product.
   The dot-product pattern should be detected.
   The reduction is currently not vectorized becaus of the signed->unsigned->signed
   casts, since this patch:

     2005-12-26  Kazu Hirata  <kazu@codesourcery.com>
                                                                                                
        PR tree-optimization/25125

   When the dot-product is detected, the loop should be vectorized on vect_sdot_qi 
   targets (targets that support dot-product of signed char).  
   This test would currently fail to vectorize on targets that support
   dot-product of chars when the accumulator is int.

   In the future could also be vectorized as widening-mult + summation,
   or with type-conversion support.
 */
short
foo2(int len) {
  int i;
  short result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}

/* char->int->int dot product. 
   Not detected as a dot-product pattern.
   Currently fails to be vectorized due to presence of type conversions. */
int
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
  int i, dot1, dot3;
  short dot2;

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

  dot3 = foo3 (N);
  if (dot3 != DOT3)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 2 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sdot_qi } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */
