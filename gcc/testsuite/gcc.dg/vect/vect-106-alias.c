/* { dg-require-effective-target vect_int } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 9

static int a[N] = {1,2,3,4,5,6,7,8,9};
static int b[N] = {2,3,4,5,6,7,8,9,0};

int main1 () {
  int i;
  int *p, *q, *p1, *q1;
  p = (unsigned int *) malloc (sizeof (unsigned int) * N);
  q = (unsigned int *) malloc (sizeof (unsigned int) * N);

  p1 = p; q1 = q;

  /* Not vectorizable: because of the redundant cast (caused by ponter
     arithmetics), alias analysis fails to distinguish between 
     the pointers.  */
  for (i = 0; i < N; i++)
    {
      *(q + i) = a[i];
      *(p + i) = b[i];
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (*q != a[i] || *p != b[i])
         abort();
       q++; 
       p++;
    }
  
  return 0; 
}

int main (void)
{ 
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "can't determine dependence" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

