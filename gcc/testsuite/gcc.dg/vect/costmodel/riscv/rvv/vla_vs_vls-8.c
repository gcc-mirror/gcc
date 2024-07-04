/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mno-vector-strict-align" } */

#include <stdint-gcc.h>

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (){
  int i,j;
  int sum,x;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += (i + j);
    }
    a[i] = sum;
  }
  return 0;
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
