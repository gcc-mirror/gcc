/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float fa[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float fb[N+4] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0};
float fc[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5};

/* Check handling of accesses for which the "initial condition" -
   the expression that represents the first location accessed - is
   more involved than just an ssa_name.  */

__attribute__ ((noinline)) int
main1 (float * __restrict__ pa, float * __restrict__ pb, float *__restrict__ pc)
{
  int i;
  float *q = pb + 4;

  for (i = 0; i < N; i++)
    {
      pa[i] = q[i] * pc[i];
    }

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (pa[i] != q[i] * pc[i])
	abort();
    }
  
  return 0;
}


int main (void)
{ 
  check_vect ();

  main1 (fa, fb, fc);

  return 0;	
}

/* For targets that don't support misaligned loads we version for the
   all three accesses (peeling to align the store will not force the
   two loads to be aligned).  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* Uncomment when this testcase gets vectorized again:
 dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail vect_no_align } } 
 dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail vect_no_align } } 
 dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" { target vect_no_align } } 
*/
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
