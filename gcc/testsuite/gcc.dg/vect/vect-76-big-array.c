/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 124
#define OFF 4

/* Check handling of accesses for which the "initial condition" -
   the expression that represents the first location accessed - is
   more involved than just an ssa_name.  */

int ib[N+OFF] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0, 1, 3, 5, 7, 11, 13, 17};
int ic[N+OFF] = {0, 1, 3, 5, 7, 11, 13, 17};

__attribute__ ((noinline))
int main1 (int *pib)
{
  int i;
  int ia[N+OFF];
  for (i = OFF; i < N+OFF; i++)
    {
      ib[i] = ib[i%8]*(i/8);
      ic[i] = ic[i%8]*(i/8);
      asm volatile ("" ::: "memory");
    }

  for (i = OFF; i < N; i++)
    {
      ia[i] = pib[i - OFF];
    }


  /* check results:  */
#pragma GCC novector
  for (i = OFF; i < N; i++)
    {
     if (ia[i] != pib[i - OFF])
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      ia[i] = pib[i - OFF];
    }


  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
     if (ia[i] != pib[i - OFF])
        abort ();
    }

  for (i = OFF; i < N; i++)
    {
      ia[i] = ic[i - OFF];
    }


  /* check results:  */
#pragma GCC novector
  for (i = OFF; i < N; i++)
    {
     if (ia[i] != ic[i - OFF])
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


/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
