/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned int result[N];

/* unsigned char-> unsigned int widening-mult.  */
__attribute__ ((noinline)) int
foo (int len) {
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

  foo (N);

  for (i=0; i<N; i++) {
    if (result[i] != X[i] * Y[i])
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_qi_to_hi || vect_unpack } } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected" 1 "vect" { target vect_widen_mult_qi_to_hi_pattern } } } */
/* { dg-final { scan-tree-dump-times "widen_mult pattern recognized" 1 "vect" { target vect_widen_mult_qi_to_hi_pattern } } } */

