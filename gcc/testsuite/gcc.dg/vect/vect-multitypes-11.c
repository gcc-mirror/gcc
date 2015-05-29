/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

short x[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) int
foo (int len, int *z) {
  int i;

  for (i=0; i<len; i++) {
    z[i] = x[i];
  }
}


int main (void)
{
  short i;
  int z[N+4];

  check_vect ();

  for (i=0; i<N; i++) {
    x[i] = i;
  }

  foo (N,z+2);

  for (i=0; i<N; i++) {
    if (z[i+2] != x[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! vect_unpack } } } } */

