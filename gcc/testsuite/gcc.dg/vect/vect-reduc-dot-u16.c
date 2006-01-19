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
   Not vectorized due to presence of type-conversions. */
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

/* short->int->int dot product. 
   Currently not detected as a dot-product pattern: the multiplication 
   promotes the ushorts to int, and then the product is promoted to unsigned 
   int for the addition.  Which results in an int->unsigned int cast, which 
   since no bits are modified in the cast should be trivially vectorizable.  */
unsigned int
foo2(int len) {
  int i;
  unsigned int result = 0;

  for (i=0; i<len; i++) {
    result += (X[i] * Y[i]);
  }
  return result;
}


int main (void)
{
  unsigned int dot1, dot2;
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

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" { xfail *-*-* } } } */

/* Once the dot-product pattern is detected in the second loop, we expect
   that loop to be vectorized on vect_udot_hi targets (targets that support 
   dot-product of unsigned shorts).  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */ 

/* { dg-final { cleanup-tree-dump "vect" } } */
