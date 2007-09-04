/* { dg-require-effective-target vect_int } */
#include <stdarg.h>
#include "tree-vect.h"


#define N 40
#define M 128
unsigned short a[M][N];
unsigned int out[N];

/* Outer-loop vectorization. */

__attribute__ ((noinline))
void
foo (){
  int i,j;
  unsigned int diff;

  for (i = 0; i < N; i++) {
    for (j = 0; j < M; j++) {
      a[j][i] = 4;
    }
    out[i]=5;
  }
}

int main (void)
{
  int i, j;
  check_vect ();

  foo ();

  for (i = 0; i < N; i++) {
    for (j = 0; j < M; j++) {
      if (a[j][i] != 4)
        abort ();
    }
    if (out[i] != 5)
      abort ();
  }

  return 0;
}


/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
