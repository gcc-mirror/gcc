/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 17

int ia[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48};
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48};
int res[N] = {12,24,36,48,60,72,84,96,108,120,132,144,156,168,180,192,48};

__attribute__ ((noinline))
int main1 ()
{
  int i;

  /* Not vectorizable due to data dependence: dependence distance 1.  */ 
  for (i = 0; i < N - 1; i++)
    {
      ia[i+1] = ia[i] * 4;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N - 1; i++)
    {
      if (ia[i] != 0)
        abort ();
    } 

  /* Vectorizable. Dependence distance -1.  */
  for (i = 0; i < N - 1; i++)
    {
      ib[i] = ib[i+1] * 4;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N - 1; i++)
    {
      if (ib[i] != res[i])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" {xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "dependence distance negative" 1 "vect"  } } */

