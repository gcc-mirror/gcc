/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=zvl -fdump-tree-vect-details" } */

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
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 1 "vect" } } */
