/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 --param riscv-autovec-preference=scalable -fselective-scheduling -fdump-tree-vect-details" } */

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
}

/* { dg-final { scan-assembler {e32,m4} } } */
/* { dg-final { scan-assembler-times {csrr} 1 } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 2" "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 1" "vect" } } */
