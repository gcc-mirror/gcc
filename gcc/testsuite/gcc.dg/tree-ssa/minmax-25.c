/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-original" } */
/* PR tree-optimization/111349 */

int f();
int g();

int test1(int a, int b)
{
        return (a > b) ? ((a > b) ? a : b) : a;
}

int test1_(int a, int b)
{
        return (b < a) ? ((a > b) ? a : b) : a;
}

/* test1 and test1_ should be able to optimize to `return a;` during fold.  */
/* { dg-final { scan-tree-dump-times "return a;" 2 "original" } } */
/* { dg-final { scan-tree-dump-not " MAX_EXPR " "original" } } */
/* { dg-final { scan-tree-dump-times "return a" 2 "optimized" } } */
