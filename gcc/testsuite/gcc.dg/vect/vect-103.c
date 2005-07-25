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
static int b[N] = {17,24,7,0,2,3,4,31,82};
static int c[N] = {9,17,24,7,0,2,3,4,31};

int main1 (int x, int y) {
  int i;
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));

  for (i = 0; i < N; i++)
    {
       p->a[i] = a[i];
       p->b[i] = b[i];
       if (x == 135)
	 abort (); /* to avoid vectorization  */
    }

  /* Vectorizable: distance > VF.  */
  for (i = 0; i < N; i++)
    {
       *((int *)p + x + i) = *((int *)p + x + i + 8);
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (p->a[i] != c[i])
         abort();
    }
  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (0, N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "dependence distance modulo vf == 0" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

