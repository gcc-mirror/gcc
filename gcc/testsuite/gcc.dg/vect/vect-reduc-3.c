/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Test vectorization of reduction of unsigned-int in the presence
   of unknown-loop-bound.  */

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

int main (void)
{ 
  check_vect ();
  
  main1 (N, 240);
  main1 (N-1, 210);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_add } } } */
