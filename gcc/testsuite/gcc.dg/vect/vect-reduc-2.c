
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 242

/* Test vectorization of reduction of signed-int.  */
/* Not supported yet.  */

int main1 (int x, int max_result)
{
  int i;
  int b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  int diff = 2;
  int max = x;
  int min = 10;

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
  
  return main1 (100, 100);
  return main1 (0, 15);
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "not vectorized: unsupported use in stmt." 3 "vect" } } */
