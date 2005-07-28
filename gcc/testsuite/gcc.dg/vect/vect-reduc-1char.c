/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 122

void
main1 (unsigned char x, unsigned char max_result, unsigned char min_result)
{
  int i;
  unsigned char ub[N] = {1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
  unsigned char uc[N] = {1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  unsigned char udiff = 2;
  unsigned char umax = x;
  unsigned char umin = x;

  for (i = 0; i < N; i++) {
    udiff += (unsigned char)(ub[i] - uc[i]);
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

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
