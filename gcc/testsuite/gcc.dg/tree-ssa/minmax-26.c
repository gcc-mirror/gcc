/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-original" } */
/* PR tree-optimization/111348 */

int test1(int a, int b, int c)
{
        return (a > b) ? ((a > c) ? a : c) : ((b > c) ? b : c);
}


int test1_(int a, int b, int c)
{
        return (b < a) ? ((a > c) ? a : c) : ((b > c) ? b : c);
}

/* test1 and test1_ should be able to optimize to `MAX_EXPR <MAX_EXPR <a, b>, c>;` during fold.  */
/* { dg-final { scan-tree-dump-times "MAX_EXPR <MAX_EXPR <a, b>, c>" 2 "original" } } */
/* { dg-final { scan-tree-dump-not "b > a" "original" } } */
/* { dg-final { scan-tree-dump-not "a > b" "original" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */

