/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT -21856

signed char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
signed char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};

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

