/* { dg-require-effective-target vect_long_long } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

char x[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};

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

  foo (N,z+2);

#pragma GCC novector
  for (i=0; i<N; i++) {
    if (z[i+2] != x[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! vect_unpack } } } } */

