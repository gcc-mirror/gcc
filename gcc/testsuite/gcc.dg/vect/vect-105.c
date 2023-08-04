/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 4

struct extraction
{
  int a[N][N];
  int b[N][N];
};

static int a[N][N] = {{1,2,3,11},{4,5,6,12},{7,8,9,13},{34,45,67,83}};
static int b[N][N] = {{17,28,15,23},{0,2,3,24},{4,31,82,25},{29,31,432,256}};
static int c[N][N] = {{1,2,3,11},{4,9,13,34},{45,67,83,13},{34,45,67,83}};

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

  /* Vectorizable: distance > number of iterations.  */
  for (i = 1; i < N; i++)
  {
#pragma GCC unroll 0
    for (j = 0; j < N; j++)
    {
       *((int *)p + x + i + j) = *((int *)p + x + i + j + 5);
    }
  }

  /* check results: */
  for (i = 0; i < N; i++)
   {
#pragma GCC novector
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 2 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "possible dependence between data-refs" 0 "vect" } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
