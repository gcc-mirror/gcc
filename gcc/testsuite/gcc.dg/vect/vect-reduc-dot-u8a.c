/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-additional-options "-march=armv8.2-a+dotprod" { target { aarch64*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT 43680

unsigned char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
unsigned char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};

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

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_udot_qi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_qi_to_hi && vect_widen_sum_qi_to_si } } } } */


