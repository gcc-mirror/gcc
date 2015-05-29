/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int ic[N*2];
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

#define ia (ic+N)

__attribute__ ((noinline))
int main1 ()
{
  int i, j;

  for (i = 0; i < N; i++)
    {
       ia[i] = ib[i];
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
       if (ia[i] != ib[i])
         abort();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
