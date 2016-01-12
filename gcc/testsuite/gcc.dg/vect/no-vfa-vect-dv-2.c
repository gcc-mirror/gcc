/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
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
  int E[N] = {0,1,2,0};

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
  for (i = 0; i < N-20; i++)
    {
      A[i] = A[i+20];
    }

  /* check results:  */
  for (i = 0; i < N-20; i++)
    {
      if (A[i] != D[i+20])
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
      C[i+3] = C[i];
    }

  /* check results:  */
  for (i = 0; i < 4; i++)
    {
      if (C[i] != E[i])
	abort ();
    }

  return 0;
}


/* The initialization induction loop (with aligned access) is also vectorized.  */
/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "accesses have the same alignment." 2 "vect" { target { { vect_aligned_arrays } && {! vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-times "accesses have the same alignment." 1 "vect" { target { {! vect_aligned_arrays } && {vect_sizes_32B_16B} } } } } */
