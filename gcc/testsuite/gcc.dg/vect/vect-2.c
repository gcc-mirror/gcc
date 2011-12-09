/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

char cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
char ca[N];

__attribute__ ((noinline)) 
int main1 ()
{  
  int i;

  for (i = 0; i < N; i++)
    {
      ca[i] = cb[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ca[i] != cb[i])
        abort ();
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
/* { dg-final { cleanup-tree-dump "vect" } } */
