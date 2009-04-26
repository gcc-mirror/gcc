/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 16

unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Reduction of unsigned-int.  */

__attribute__ ((noinline))
int main1 (int n, int res)
{
  int i;
  unsigned int udiff;

  udiff = 0;
  for (i = 0; i < n; i++) {
    udiff += (ub[i] - uc[i]);
  }

  /* check results:  */
  if (udiff != res)
    abort ();

  return 0;
}

int main (void)
{ 
  main1 (N, 240);
  main1 (N-1, 210);
  return 0;
}

/* { dg-final { scan-tree-dump-times "Detected reduction" 1 "parloops" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

