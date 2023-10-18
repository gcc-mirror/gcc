/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fdump-tree-vect-details" } */

int
bar (int *x, int a, int b, int n)
{
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  int sum1 = 0;
  int sum2 = 0;
  for (int i = 0; i < n; ++i)
    {
      sum1 += x[2*i] - a;
      sum1 += x[2*i+1] * b;
      sum2 += x[2*i] - b;
      sum2 += x[2*i+1] * a;
    }
  return sum1 + sum2;
}

/* { dg-final { scan-assembler {e32,m2} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-times {ret} 2 } } *
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 1" "vect" } } */
