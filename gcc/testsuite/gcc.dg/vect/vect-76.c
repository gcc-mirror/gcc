/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8
#define OFF 4

typedef int aint __attribute__ ((__aligned__(16)));

aint ib[N+OFF] = {0, 1, 3, 5, 7, 11, 13, 17, 0, 2, 6, 10};

int main1 (aint *pib)
{
  int i;
  int ia[N+OFF];
  int ic[N+OFF] = {0, 1, 3, 5, 7, 11, 13, 17, 0, 2, 6, 10};

  for (i = OFF; i < N; i++)
    {
      ia[i] = pib[i - OFF];
    }


  /* check results:  */
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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */

