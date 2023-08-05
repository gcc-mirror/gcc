/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int result[N];

/* short->int widening-mult */
__attribute__ ((noinline)) int
foo1(int len) {
  int i;

  for (i=0; i<len; i++) {
    result[i] = X[i] * Y[i];
  }
}

int main (void)
{
  int i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    __asm__ volatile ("");
  }

  foo1 (N);

#pragma GCC novector
  for (i=0; i<N; i++) {
    if (result[i] != X[i] * Y[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_mult_hi_to_si } } } */

