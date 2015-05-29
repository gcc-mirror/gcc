/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 320
#define M 1024
float in[N+M];
float out[N];

/* Outer-loop vectorization.  */

__attribute__ ((noinline)) void
foo (){
  int i,j;
  float diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=4) {
      diff += in[j+i];
    }
    out[i]=diff;
  }
}

int main (void)
{
  check_vect ();
  int i, j;
  float diff;

  for (i = 0; i < N; i++)
    in[i] = i;

  foo ();

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=4) {
      diff += in[j+i];
    }
    if (out[i] != diff)
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect"  } } */
