/* { dg-require-effective-target vect_float } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 40
float image[N][N+1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float out[N];

/* Outer-loop vectorization.  */

__attribute__ ((noinline)) void
foo (){
  int i,j;
  float diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < N; j+=4) {
      diff += image[j][i];
    }
    out[i]=diff;
  }
}

int main (void)
{
  check_vect ();
  int i, j;
  float diff;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      image[i][j]=i+j;
    }
  }

  foo ();

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < N; j+=4) {
      diff += image[j][i];
    }
    if (out[i] != diff)
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
