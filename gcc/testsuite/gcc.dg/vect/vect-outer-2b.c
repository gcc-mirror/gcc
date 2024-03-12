/* { dg-require-effective-target vect_float } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 40
float image[2*N][N][N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) void
foo (){
  int i,j,k;

 for (k=0; k<N; k++) {
  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      image[k+i][j][i] = j+i+k;
    }
  }
 }
}

int main (void)
{
  check_vect ();
  int i, j, k;

  foo ();

 for (k=0; k<N; k++) {
  for (i = 0; i < N; i++) {
#pragma GCC novector
    for (j = 0; j < N; j++) {
      if (image[k+i][j][i] != j+i+k)
	abort ();
    }
  }
 }

  return 0;
}

/* { dg-final { scan-tree-dump-times "grouped access in outer loop" 1 "vect" } } */
