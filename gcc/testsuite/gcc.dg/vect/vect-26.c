/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* unaligned store.  */

int main1 ()
{
  int i;
  int ia[N+1];

  for (i = 1; i <= N; i++)
    {
      ia[i] = 5;
    }

  /* check results:  */
  for (i = 1; i <= N; i++)
    {
      if (ia[i] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */

