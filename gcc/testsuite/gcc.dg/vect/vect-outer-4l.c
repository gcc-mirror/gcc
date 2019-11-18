/* { dg-require-effective-target vect_int } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 96
#define M 128
unsigned short in[N+M];
unsigned int out[N];
unsigned char arr[N];

/* Outer-loop vectorization. */
/* Not vectorized due to multiple-types in the inner-loop.  */

__attribute__ ((noinline)) unsigned int
foo (){
  int i,j;
  unsigned int diff;
  unsigned int s=0;

  for (i = 0; i < N; i++) {
    arr[i] = 3;
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    s+=diff;
  }
  return s;
}

__attribute__ ((noinline)) unsigned int
bar (int i, unsigned int diff, unsigned short *in)
{
    int j;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    return diff;
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
    arr[i] = 3;
    diff = 0;
    diff = bar (i, diff, in);
    s += diff;
  }

  if (s != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail { ! aarch64*-*-* } } } }*/
