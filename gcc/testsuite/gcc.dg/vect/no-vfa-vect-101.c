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
int main1 (int x, int y) {
  int i;
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));

  /* Not vectorizable: different unknown offset.  */
#pragma GCC unroll 0
  for (i = 0; i < N; i++)
    {
      *((int *)p + x + i) = a[i];
      *((int *)p + y + i) = b[i];
    }

  /* check results: */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
       if (p->a[i] != a[i] || p->b[i] != b[i])
         abort();
    }
  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (0, N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "can't determine dependence" 1 "vect" { target { ! vect_multiple_sizes } } } } */
/* { dg-final { scan-tree-dump "can't determine dependence" "vect" { target vect_multiple_sizes } } } */

