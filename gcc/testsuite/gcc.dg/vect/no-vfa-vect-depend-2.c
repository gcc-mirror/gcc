/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 17

int ia[N] = {48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3,0};
int ib[N] = {48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3,0};
int res[N] = {48,192,180,168,156,144,132,120,108,96,84,72,60,48,36,24,12};

__attribute__ ((noinline))
int main1 ()
{
  int i;

  /* Not vectorizable due to data dependence: dependence distance 1.  */ 
  for (i = N - 2; i >= 0; i--)
    {
      ia[i] = ia[i+1] * 4;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != 0)
	abort ();
    } 

  /* Vectorizable. Dependence distance -1.  */
  for (i = N - 2; i >= 0; i--)
    {
      ib[i+1] = ib[i] * 4;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ib[i] != res[i])
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" {xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "dependence distance negative" 1 "vect"  } } */
