/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param=vect-epilogues-nomask=0" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT1 21834 
#define DOT2 21876

unsigned short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* short->short->int dot product. 
   Not detected as a dot-product pattern.
   Requires support for non-widneing multiplication and widening-summation.  
   Vectorized with loop-aware SLP. */
__attribute__ ((noinline)) unsigned int
foo1(int len, int *result1, int *result2) 
{
  int i;
  unsigned int res1 = 10, res2 = 20;
  unsigned short prod;

  for (i=0; i<len; i++) {
    prod = X[2*i] * Y[2*i];
    res1 += prod;
    prod = X[2*i+1] * Y[2*i+1];
    res2 += prod;
  }

  *result1 = res1;
  *result2 = res2;

  return 0;
}

int main (void)
{
  unsigned int dot1, dot2;
  unsigned short i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    asm volatile ("" ::: "memory");
  }

  foo1 (N/2, &dot1, &dot2);

  if (dot1 != DOT1 || dot2 != DOT2)
    abort ();

  return 0;
}

/* The initialization loop in main also gets vectorized.  */
/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_short_mult && { vect_widen_sum_hi_to_si  && vect_unpack } } } } } */ 
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail { vect_widen_sum_hi_to_si_pattern || { ! { vect_short_mult && { vect_widen_sum_hi_to_si  && vect_unpack } } } } } } } */
/* Check we can elide permutes if SLP vectorizing the reduction.  */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" { xfail { { { vect_widen_sum_hi_to_si_pattern || { ! vect_unpack } } && { ! vect_load_lanes } } && { vect_short_mult && { vect_widen_sum_hi_to_si  && vect_unpack } } } } } } */
