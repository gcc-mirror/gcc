/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-final_cleanup" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 16
#define DIFF 242

__attribute__ ((noinline))
int main1 (float x, float max_result)
{
  int i;
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float diff = 2;
  float max = x;
  float min = 10;

  for (i = 0; i < N; i++) {
    diff += (b[i] - c[i]);
  }

  for (i = 0; i < N; i++) {
    max = max < c[i] ? c[i] : max;
  }

  for (i = 0; i < N; i++) {
    min = min > c[i] ? c[i] : min;
  }

  /* check results:  */
  if (diff != DIFF)
    abort ();
  if (max != max_result)
    abort ();
  if (min != 0)
    abort ();

  return 0;
}

int main (void)
{ 
  main1 (100 ,100);
  main1 (0, 15);
  return 0;
}

/* need -ffast-math to  parallelize these loops.  */
/* { dg-final { scan-tree-dump-times "Detected reduction" 0 "parloops" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 0 "parloops" } } */
/* { dg-final { scan-tree-dump-times "FAILED: it is not a part of reduction" 3 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
