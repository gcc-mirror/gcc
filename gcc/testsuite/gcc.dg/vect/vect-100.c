/* { dg-require-effective-target vect_int } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 9

struct extraction
{
  int a[N];
  int b[N];
};

static int a[N] = {1,2,3,4,5,6,7,8,9};
static int b[N] = {2,3,4,5,6,7,8,9,0};

__attribute__ ((noinline))
int main1 () {
  int i;
  struct extraction *p;
  
  p = (struct extraction *) malloc (sizeof (struct extraction));

  /* Vectorizable: alias analysis determines that p can't point to a and/or b.  */
  for (i = 0; i < N; i++)
    {
      p->a[i] = a[i];
      p->b[i] = b[i];
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (p->a[i] != a[i] || p->b[i] != b[i])
         abort();
    }

  return 0;
}

__attribute__ ((noinline))
int main2 () {
  int i;
  int c[N] = {1,2,3,4,5,6,7,8,9};
  int d[N] = {2,3,4,5,6,7,8,9,0};
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));

  /* Vectorizable: c and d are local arrays.  */
  for (i = 0; i < N; i++)
    {
      p->a[i] = c[i];
      p->b[i] = d[i];
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (p->a[i] != c[i] || p->b[i] != d[i])
         abort();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  main1 ();
  main2 ();
  
  return 0;	
}

/* Requires versioning.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

