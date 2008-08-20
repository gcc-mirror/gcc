/* { dg-require-effective-target vect_long_long } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

char x[N] __attribute__ ((__aligned__(16)));

__attribute__ ((noinline)) int
foo (int len, long long *z) {
  int i;

  for (i=0; i<len; i++) {
    z[i] = x[i];
  }
}


int main (void)
{
  char i;
  long long z[N+4];

  check_vect ();

  for (i=0; i<N; i++) {
    x[i] = i;
    if (i % 5)
      x[i] = i;
  }

  foo (N,z+2);

  for (i=0; i<N; i++) {
    if (z[i+2] != x[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! vect_unpack } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

