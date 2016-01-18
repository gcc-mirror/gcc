/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 1600
#define DIFF 2558400

int b[N];
int c[N];

/* Reduction of signed-int.  */

__attribute__ ((noinline))
void main1 (int x, int max_result, int min_result)
{
  int i;
  int diff = 0;
  int max = x;
  int min = x;

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
  if (min != min_result)
    abort ();
}

 __attribute__((noinline))
 void init_arrays ()
 {
   int i;

   b[0] = 1;
   c[0] = 1;
   for (i=1; i<N; i++)
     {
       b[i] = i * 3;
       c[i] = i;
     }
}

int main (void)
{ 
  init_arrays ();
  main1 (2000, 2000, 1);
  main1 (0, 1599, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "Detected reduction" 3 "parloops2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 4 "parloops2" { xfail *-*-* } } } */

