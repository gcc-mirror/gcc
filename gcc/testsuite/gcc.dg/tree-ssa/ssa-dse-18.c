/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-optimized" } */
int g(_Complex int*);
int f(void)
{
  _Complex int t = 0;
  int i, j;
 __imag__ t += 2;
  return g(&t);
}


/* { dg-final { scan-tree-dump-times "__complex__" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "REALPART_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "IMAGPART_EXPR" 1 "optimized" } } */
