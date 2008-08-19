/* { dg-require-effective-target vect_long_long } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned char uX[N] __attribute__ ((__aligned__(16)));
unsigned char uresultX[N];
unsigned long long uY[N] __attribute__ ((__aligned__(16)));
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

  for (i=0; i<N; i++) {
    uX[i] = 16-i;
    uY[i] = 16-i;
    if (i%5 == 0)
      uX[i] = 16-i;
  }

  foo1 (N);

  for (i=0; i<N; i++) {
    if (uresultX[i] != uX[i])
      abort ();
    if (uresultY[i] != (unsigned char)uY[i])
      abort ();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_pack_trunc } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

