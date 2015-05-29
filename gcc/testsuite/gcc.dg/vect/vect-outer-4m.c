/* { dg-require-effective-target vect_int } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 40
#define M 128
unsigned short in[N+M];
unsigned int out[N];

/* Outer-loop vectorization. */
/* Not vectorized due to multiple-types in the inner-loop.  */

__attribute__ ((noinline)) unsigned int
foo (){
  int i,j;
  unsigned int diff;
  unsigned int s=0;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    s+=((unsigned short)diff>>3);
  }
  return s;
}

int main (void)
{
  int i, j;
  unsigned int diff;
  unsigned int s=0,sum=0;

  check_vect ();

  for (i = 0; i < N+M; i++) {
    in[i] = i;
  }

  sum=foo ();

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    s += ((unsigned short)diff>>3);
  }

  if (s != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect"  { xfail *-*-* } } } */
