/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 242

/* Test vectorization of reduction of unsigned-int.  */

void main1 (unsigned int x, unsigned int max_result, unsigned int min_result)
{
  int i;
  unsigned int ub[N] = {1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  unsigned int uc[N] = {1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  unsigned int udiff = 2;
  unsigned int umax = x;
  unsigned int umin = x;

  /* Summation.  */
  for (i = 0; i < N; i++) {
    udiff += (ub[i] - uc[i]);
  }

  /* Maximum.  */
  for (i = 0; i < N; i++) {
    umax = umax < uc[i] ? uc[i] : umax;
  }

  /* Minimum.  */
  for (i = 0; i < N; i++) {
    umin = umin > uc[i] ? uc[i] : umin;
  }

  /* check results:  */
  if (udiff != DIFF)
    abort ();
  if (umax != max_result)
    abort ();
  if (umin != min_result)
    abort ();
}

int main (void)
{ 
  check_vect ();
  
  main1 (100, 100, 1);
  main1 (0, 15, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail { vect_no_int_add || vect_no_int_max } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
