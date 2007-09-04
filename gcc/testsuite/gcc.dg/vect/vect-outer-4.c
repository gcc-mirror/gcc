/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40
#define M 128
float in[N+M];
float coeff[M];
float out[N];

/* Outer-loop vectorization.  */

__attribute__ ((noinline)) void
foo (){
  int i,j;
  float diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=4) {
      diff += in[j+i]*coeff[j]; 
    }
    out[i]=diff;
  }
}

int main (void)
{
  check_vect ();
  int i, j;
  float diff;

  for (i = 0; i < M; i++)
    coeff[i] = i;
  for (i = 0; i < N+M; i++)
    in[i] = i;

  foo ();
  
  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=4) {
      diff += in[j+i]*coeff[j];
    }
    if (out[i] != diff)
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "zero step in outer loop." 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
