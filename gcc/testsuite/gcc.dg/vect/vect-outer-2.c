/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_intfloat_cvt } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 40
float image[N][N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float out[N];

/* Outer-loop vectorization.  */

__attribute__ ((noinline)) void
foo (){
  int i,j;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      image[j][i] = j+i;
    }
  }
}

int main (void)
{
  check_vect ();
  int i, j;

  foo ();

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      if (image[j][i] != j+i)
	abort ();
    }
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
