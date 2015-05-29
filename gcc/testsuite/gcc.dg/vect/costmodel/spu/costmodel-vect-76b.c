/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 16
#define OFF 4

/* Check handling of accesses for which the "initial condition" -
   the expression that represents the first location accessed - is
   more involved than just an ssa_name.  */

int ib[N+OFF] __attribute__ ((__aligned__(16))) = {0, 1, 3, 5, 7, 11, 13, 17, 0, 2, 6, 10, 0, 1, 3, 5, 7, 11, 13, 17};
int ic[N+OFF] = {0, 1, 3, 5, 7, 11, 13, 17, 0, 2, 6, 10, 0, 1, 3, 5, 7, 11, 13, 17};

int main1 (int *pib)
{
  int i;
  int ia[N+OFF];

  for (i = OFF; i < N; i++)
    {
      pib[i - OFF] = ic[i];
    }


  /* check results:  */
  for (i = OFF; i < N; i++)
    {
     if (pib[i - OFF] != ic[i])
        abort ();
    }

  return 0;  
}

int main (void)
{
  check_vect ();

  main1 (&ib[OFF]);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorization not profitable" 0 "vect" } } */
