/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dce -fdump-tree-optimized" } */
_Complex int t = 0;
int f(void)
{
  t = 0;
 __imag__ t = 2;
}

/* { dg-final { scan-tree-dump-times "__complex__" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "REALPART_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "IMAGPART_EXPR" 1 "optimized" } } */

