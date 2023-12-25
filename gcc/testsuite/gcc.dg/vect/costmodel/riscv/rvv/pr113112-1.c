/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize --param riscv-autovec-lmul=dynamic --param riscv-autovec-preference=fixed-vlmax -fdump-tree-vect-details" } */

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (int n){
  int i,j;
  int sum,x;

  for (i = 0; i < n; i++) {
    sum = 0;
    for (j = 0; j < n; j++) {
      sum += (i + j);
    }
    a[i] = sum;
  }
  return 0;
}

/* { dg-final { scan-assembler {e32,m4} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-times {ret} 1 } } */
/* { dg-final { scan-tree-dump "Maximum lmul = 8" "vect" } } */
/* { dg-final { scan-tree-dump "Maximum lmul = 4" "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 2" "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 1" "vect" } } */
/* { dg-final { scan-tree-dump "At most 8 number of live V_REG at program point 0 for bb 4" "vect" } } */
/* { dg-final { scan-tree-dump "At most 40 number of live V_REG at program point 0 for bb 3" "vect" } } */
/* { dg-final { scan-tree-dump "At most 8 number of live V_REG at program point 0 for bb 5" "vect" } } */
