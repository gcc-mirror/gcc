/* { dg-require-effective-target vect_int } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct extraction
{
  int a[N][N];
  int b[N][N];
};

static int a[N][N];
static int b[N][N];
static int c[N][N];

volatile int y;

__attribute__ ((noinline))
int main1 (int x) {
  int i,j, off;
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));
  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       a[i][j] = (i*7 + j*17)%53;
       b[i][j] = (i*11+ j*13)%41;
       if (y)
	 abort (); /* to avoid vectorization.  */
     }
   }
  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       c[i][j] = a[i][j];
       if (y)
	 abort (); /* to avoid vectorization.  */
     }
   }
  for (i = 1; i < N; i++)
  {
    for (j = 0; j < N; j++)
    {
      off = x + i + j + N+1;
      if (x + i + j > N*N-1)
	break;
      if (off > N*N-1)
	*(&c[0][0]+x+i+j) = *(&b[0][0] + off - N*N);
      else
	*(&c[0][0]+x+i+j) = *(&a[0][0] + off);
       if (y)
	 abort (); /* to avoid vectorization.  */
    }
  }

  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       p->a[i][j] = a[i][j];
       p->b[i][j] = b[i][j];
       /* Because Y is volatile, the compiler cannot move this check out
	  of the loop.  */
       if (y)
	 abort (); /* to avoid vectorization.  */
     }
   }

  /* Vectorizable: distance > number of iterations.  */
  for (i = 1; i < N; i++)
  {
    for (j = 0; j < N; j++)
    {
       *((int *)p + x + i + j) = *((int *)p + x + i + j + N+1);
    }
  }

  /* check results: */
  for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
     {
       if (p->a[i][j] != c[i][j])
         abort ();
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

