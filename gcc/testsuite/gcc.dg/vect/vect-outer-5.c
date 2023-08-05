/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define MAX 42

extern void abort(void); 

__attribute__ ((noinline)) 
int main1 ()
{  
  float A[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float B[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float C[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float D[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float E[4] = {0,480,960,1440};
  float s;

  int i, j;

  for (i = 0; i < N; i++)
    {
      A[i] = i;
      B[i] = i;
      C[i] = i;
      D[i] = i;
    }

  /* Outer-loop 1: Vectorizable with respect to dependence distance. */
  for (i = 0; i < N-20; i++)
    {
      s = 0;
      for (j=0; j<N; j+=4)
        s += C[j];
      A[i] = A[i+20] + s;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N-20; i++)
    {
      s = 0;
      for (j=0; j<N; j+=4)
        s += C[j];
      if (A[i] != D[i+20] + s)
        abort ();
    }

  /* Outer-loop 2: Not vectorizable because of dependence distance. */
  for (i = 0; i < 4; i++)
    {
      s = 0;
      for (j=0; j<N; j+=4)
	s += C[j];
      B[i+1] = B[i] + s;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < 4; i++)
    {
      if (B[i] != E[i])
	abort ();
    }

  return 0;
}

int main ()
{
  check_vect ();
  return main1();
}

/* NOTE: We temporarily xfail the following check until versioning for
   aliasing is fixed to avoid versioning when the dependence distance
   is known.  */
/* { dg-final { scan-tree-dump-times "not vectorized: possible dependence between data-refs" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { scan-tree-dump "zero step in outer loop." "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
