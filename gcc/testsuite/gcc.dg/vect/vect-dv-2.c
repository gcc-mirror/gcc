/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <signal.h>
#include "tree-vect.h"

#define N 64
#define MAX 42

extern void abort(void); 

int main ()
{  
  int A[N];
  int B[N];
  int C[N];
  int D[N];

  int i, j;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      A[i] = i;
      B[i] = i;
      C[i] = i;
      D[i] = i;
    }

  /* Vectorizable */
  for (i = 0; i < 16; i++)
    {
      A[i] = A[i+20];
    }

  /* check results:  */
  for (i = 0; i < 16; i++)
    {
      if (A[i] != A[i+20])
	abort ();
    }

  /* Vectorizable */
  for (i = 0; i < 16; i++)
    {
      B[i] = B[i] + 5;
    }

  /* check results:  */
  for (i = 0; i < 16; i++)
    {
      if (B[i] != C[i] + 5)
	abort ();
    }

  /* Not vectorizable */
  for (i = 0; i < 4; i++)
    {
      C[i] = C[i+3];
    }

  /* check results:  */
  for (i = 0; i < 4; i++)
    {
      if (C[i] != D[i+3])
	abort ();
    }

  return 0;
}



/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "accesses have the same alignment." 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
