/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
int main1 ()
{
  int i;
  int ia[N];

  /* Induction.  */
  for ( i = 0; i < N; i++) {
    ia[i] = i;
  }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != i)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
