/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 242

unsigned short ub[N] = {1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned short uc[N] = {1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) void
main1 (unsigned short x, unsigned short max_result, unsigned short min_result)
{
  int i;
  unsigned short  udiff = 2;
  unsigned short umax = x;
  unsigned short umin = x;

  for (i = 0; i < N; i++) {
    udiff += (unsigned short)(ub[i] - uc[i]);
  }

  for (i = 0; i < N; i++) {
    umax = umax < uc[i] ? uc[i] : umax;
  }

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

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail vect_no_int_max } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
