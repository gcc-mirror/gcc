/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 43680
#define DOT2 43680
#define DOT3 43680

unsigned char X[N] __attribute__ ((__aligned__(16)));
unsigned char Y[N] __attribute__ ((__aligned__(16)));

/* char->short->int dot product. 
   Detected as a dot-product pattern.
   Should be vectorized on targets that support dot-product for unsigned chars.
   */
unsigned int
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

/* char->short->short dot product. 
   Detected as a dot-product pattern.
   Should be vectorized on targets that support dot-product for unsigned chars.
   This test currently fails to vectorize on targets that support dot-product 
   of chars only when the accumulator is int.
   */
unsigned short
foo2(int len) {
  int i;
  unsigned short result = 0;

  for (i=0; i<len; i++) {
    result += (unsigned short)(X[i] * Y[i]);
  }
  return result;
}

/* char->int->int dot product. 
   Not detected as a dot-product.
   Doesn't get vectorized due to presence of type converisons.  */
unsigned int
foo3(int len) {
  int i;
  unsigned int result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}

int main (void)
{
  unsigned int dot1, dot3;
  unsigned short dot2;
  int i;

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

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 2 "vect" } } */

/* When the vectorizer is enhanced to vectorize foo2 (accumulation into short) for 
   targets that support accumulation into int (powerpc, ia64) we'd have:
dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_udot_qi } }
*/
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_udot_qi } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */

