/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"
#include <stdio.h>

#define N 64

#define DOT1 43680
#define DOT2 -20832

signed char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned char CX[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) void
foo1(int len) {
  int i;
  int result1 = 0;

  for (i=0; i<len; i++) {
    result1 += (X[i] * Y[i]);
    CX[i] = 5;
  }

  if (result1 != DOT1)
    abort ();
}


int main (void)
{
  int i, dot1, dot2;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = i;
    Y[i] = 64-i;
    CX[i] = i;
    __asm__ volatile ("");
  }

  foo1 (N);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_sdot_hi  || vect_unpack } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

