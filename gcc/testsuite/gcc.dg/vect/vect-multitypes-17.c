/* { dg-require-effective-target vect_long_long } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned char uX[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};
unsigned char uresultX[N];
unsigned long long uY[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};
unsigned char uresultY[N];

/* Unsigned type demotion (si->qi) */

__attribute__ ((noinline)) int
foo1(int len) {
  int i;

  for (i=0; i<len; i++) {
    uresultX[i] = uX[i];
    uresultY[i] = (unsigned char)uY[i];
  }
}

int main (void)
{
  int i;

  check_vect ();

  foo1 (N);

#pragma GCC novector
  for (i=0; i<N; i++) {
    if (uresultX[i] != uX[i])
      abort ();
    if (uresultY[i] != (unsigned char)uY[i])
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_pack_trunc } } } */

