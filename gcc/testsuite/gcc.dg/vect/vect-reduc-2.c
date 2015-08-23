/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 240

int b[N] = {1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int c[N] = {1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Test vectorization of reduction of signed-int.  */

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

int main (void)
{ 
  check_vect ();
  
  main1 (100, 100, 1);
  main1 (0, 15, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail { vect_no_int_add || vect_no_int_min_max } } } } */
