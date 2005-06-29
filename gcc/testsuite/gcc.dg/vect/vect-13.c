/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int a[N];
int results[N] = {0,1,2,3,0,0,0,0,0,0,0,0,12,13,14,15};

int main1()
{
  int i;
  int b[N] = {0,1,2,3,-4,-5,-6,-7,-8,-9,-10,-11,12,13,14,15};

  /* Max pattern.  */
  for (i = 0; i < N; i++)
    {
      a[i] = (b[i] >= 0 ? b[i] : 0);
    }

  /* Check results  */
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_max } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
