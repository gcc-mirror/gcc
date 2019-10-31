/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define N (VECTOR_BITS / 16 + 10)
#else
#define N 26
#endif
 
__attribute__ ((noinline))
unsigned int main1 ()
{  
  unsigned short i;
  unsigned int intsum = 0;

  /* vectorization of reduction with induction, and widenning sum: 
     sum shorts into int. 
     Need -fno-tree-scev-cprop or else the loop is eliminated.  */
  for (i = 0; i < N; i++)
    {
      intsum += i;
    } 

  return intsum;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_sum_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_sum_pattern: detected" 1 "vect" { target vect_widen_sum_hi_to_si } } } */
