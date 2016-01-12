/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define MAX 42

float A[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float B[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float C[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float D[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
extern void abort(void); 

__attribute__ ((noinline))
int main1 ()
{  
  float s;

  int i, j;

  for (i = 0; i < N; i++)
    {
      s = 0;
      for (j = 0; j < N; j += 4)
	s += C[j];
      A[i] = s;
    }

  return 0;
}

int main ()
{
  int i,j;
  float s;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      A[i] = i;
      B[i] = i;
      C[i] = i;
      D[i] = i;
    }

  main1();

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      s = 0;
      for (j = 0; j < N; j += 4)
        s += C[j];
      if (A[i] != s)
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "zero step in outer loop." 1 "vect" } } */
