/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned char uX[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
unsigned short uresult[N];
signed char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
short result[N];

/* Unsigned type promotion (qi->hi) */
__attribute__ ((noinline)) int
foo1(int len) {
  int i;

  for (i=0; i<len; i++) {
    uresult[i] = (unsigned short)uX[i];
  }
}

/* Signed type promotion (qi->hi) */
__attribute__ ((noinline)) int
foo2(int len) {
  int i;

  for (i=0; i<len; i++) {
    result[i] = (short)X[i];
  }
}

int main (void)
{
  int i;

  check_vect ();

  for (i=0; i<N; i++) {
    X[i] = 16-i;
    uX[i] = 16-i;
    __asm__ volatile ("");
  }

  foo1 (N);

#pragma GCC novector
  for (i=0; i<N; i++) {
    if (uresult[i] != (unsigned short)uX[i])
      abort ();
  }
  
  foo2 (N);
  
#pragma GCC novector
  for (i=0; i<N; i++) {
    if (result[i] != (short)X[i])
      abort ();
  }
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_unpack } } } */

