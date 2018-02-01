/* { dg-require-effective-target vect_float_strict } */
/* { dg-additional-options "-fno-fast-math" } */

#include <stdarg.h>
#include "tree-vect.h"

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
  check_vect ();
  
  main1 (100 ,100);
  main1 (0, 15);
  return 0;
}

/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
