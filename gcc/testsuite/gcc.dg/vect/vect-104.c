/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param max-completely-peel-times=1 -fdump-tree-optimized-details-blocks" } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 3

struct extraction
{
  int a[N][N];
  int b[N][N];
};

static int a[N][N] = {{1,2,3},{4,5,6},{7,8,9}};
static int b[N][N] = {{17,24,7},{0,2,3},{4,31,82}};
static int c[N][N] = {{1,2,3},{4,5,5},{5,5,5}};

__attribute__ ((noinline))
int main1 (int x) {
  int i,j;
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));

  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       p->a[i][j] = a[i][j];
       p->b[i][j] = b[i][j];
       asm volatile ("" ::: "memory");
     }
   }

  /* Not vectorizable: distance = 1.  */
  for (i = 1; i < N; i++)
  {
    for (j = 0; j < N; j++)
    {
       *((int *)p + x + i + j + 1) = *((int *)p + x + i + j);
    }
  }

  /* check results: */
#pragma GCC novector
  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       if (p->a[i][j] != c[i][j])
         abort();
     }
  }
  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "possible dependence between data-refs" 1 "vect" { target { ! vect_multiple_sizes } } } } */
/* { dg-final { scan-tree-dump "possible dependence between data-refs" "vect" { target vect_multiple_sizes } } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
