/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 1600
#define DIFF 2558402


__attribute__ ((noinline))
int main1 (float x, float max_result)
{
  int i;
  float b[N];
  float c[N];
  float diff = 2;
  float max = x;
  float min = 10;

  for (i=0; i<N; i++)
    {
      b[i] = i * 3;
      c[i] = i;
    }
  
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
  main1 (2000, 2000);
  main1 (0, 1599);
  return 0;
}


/* need -ffast-math to  parallelize these loops.  */
/* { dg-final { scan-tree-dump-times "Detected reduction" 0 "parloops" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops" } } */
/* { dg-final { scan-tree-dump-times "FAILED: it is not a part of reduction" 3 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
