/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};

int result[N];

/* short->int widening-mult */
__attribute__ ((noinline)) int
foo1(int len) {
  int i;

  for (i=0; i<len/2; i++) {
    result[2*i] = X[2*i] * Y[2*i];
    result[2*i+1] = X[2*i+1] * Y[2*i+1];
  }
}

int main (void)
{
  int i;

  check_vect ();

  foo1 (N);

  for (i=0; i<N; i++) {
    if (result[i] != X[i] * Y[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_hi_to_si || vect_unpack } } } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_widen_mult_hi_to_si || vect_unpack } } } } */

