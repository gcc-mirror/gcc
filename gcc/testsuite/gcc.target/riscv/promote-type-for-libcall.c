/* { dg-do run } */
/* { dg-options "-ftree-slp-vectorize -funroll-loops" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include <stdio.h>
#include <stdlib.h>
#define N 4
volatile float f[N];
int x[N] __attribute__((aligned(8)));
int main() {
  int i;
  x[0] = -1;
  x[1] = 2;
  x[2] = -2;
  x[3] = 2;

  for (i=0;i<N;++i){
    f[i] = x[i];
  }

  if (f[0] != -1.0f) {
    abort();
  }

  if (f[1] != 2.0f) {
    abort();
  }

  if (f[2] != -2.0f) {
    abort();
  }

  if (f[3] != 2.0f) {
    abort();
  }

  return 0;
}
