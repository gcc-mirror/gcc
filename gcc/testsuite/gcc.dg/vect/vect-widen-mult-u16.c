/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned int result[N];

/* short->int widening-mult */
__attribute__ ((noinline)) int
foo1(int len) {
  int i;

  /* Not vectorized because X[i] and Y[i] are casted to 'int'
     so the widening multiplication pattern is not recognized.  */
  for (i=0; i<len; i++) {
    result[i] = (unsigned int)(X[i] * Y[i]);
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

  for (i=0; i<N; i++) {
    if (result[i] != X[i] * Y[i])
      abort ();
  }
  
  return 0;
}

/*The induction loop is vectorized  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_pack_trunc } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

