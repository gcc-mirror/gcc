/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int main1 ()
{
  int i,j;
  int ia[N];

  /* Induction.  */
  for (j=0,i=N;  j<N,i>0;  i--,j++) {
    ia[j] = i;
  }

  /* check results:  */
  for (j=0,i=N;  j<N,i>0;  i--,j++) {
      if (ia[j] != i)
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
/* { dg-final { cleanup-tree-dump "vect" } } */
