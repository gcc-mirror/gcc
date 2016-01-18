/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 1600

unsigned int ub[N];
unsigned int uc[N];

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

__attribute__((noinline))
void init_arrays ()
{
  int i;
  
  for (i=0; i<N; i++)
    {
      ub[i] = i * 3;
      uc[i] = i;
    }
}

int main (void)
{ 
  init_arrays ();
  main1 (N, 2558400);
  main1 (N-1, 2555202);
  return 0;
}


/* { dg-final { scan-tree-dump-times "Detected reduction" 1 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 2 "parloops2" } } */

