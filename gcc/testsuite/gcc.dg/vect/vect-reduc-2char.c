/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 121

int main1 (char x, char max_result)
{
  int i;
  char b[N] = {0,2,3,6,8,10,12,14,16,18,20,22,24,26,28,30};
  char c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  signed char diff = 2;
  char max = x;
  char min = 10;

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
  
  main1 (100, 100);
  main1 (0, 15);
  return 0 ;
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
