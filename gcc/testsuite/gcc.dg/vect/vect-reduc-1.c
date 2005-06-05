/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 242

/* Test vectorization of reduction of unsigned-int.  */
/* Not supported yet.  */

int main1 (unsigned int x, unsigned int max_result)
{
  int i;
  unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  unsigned int udiff = 2;
  unsigned int umax = x;
  unsigned int umin = 10;

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
  if (umin != 0)
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
