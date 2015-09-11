/* { dg-require-effective-target vect_int } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 9

static int a[N] = {1,2,3,4,5,6,7,8,9};
static int b[N] = {2,3,4,5,6,7,8,9,0};

__attribute__ ((noinline))
int main1 () {
  int i;
  int *p, *q, *p1, *q1;
  p = (unsigned int *) malloc (sizeof (unsigned int) * N);
  q = (unsigned int *) malloc (sizeof (unsigned int) * N);

  p1 = p; q1 = q;

  /* Vectorizable, before pointer plus we would get a redundant cast
     (caused by pointer arithmetics), alias analysis fails to distinguish
     between the pointers.  */
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
  
  q = q1;
  p = p1;
  /* Vectorizable.  */ 
  for (i = 0; i < N; i++)
    {
      *q = b[i];
      *p = a[i];
      q++;
      p++;
    }

  q = q1;
  p = p1;
  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (*q != b[i] || *p != a[i])
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

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */

