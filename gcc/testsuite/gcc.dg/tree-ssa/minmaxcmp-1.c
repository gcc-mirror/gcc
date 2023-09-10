/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-original" } */
/* PR tree-optimization/111346 */

int f();
int g();

_Bool test1(int a, int b)
{
        return ((a > b) ? a : b) >= a; // return 1;
}
_Bool test1_(int a, int b)
{
        return a <= ((a > b) ? a : b); // return 1;
}
/* test1 and test1_ should be able to optimize to `return 1;` during fold.  */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "original" } } */
/* { dg-final { scan-tree-dump-not " MAX_EXPR " "original" } } */

_Bool test2(int a, int b)
{
        a = f();
        a = g();
        int t = a;
        if (t < b) t = b;
        return t >= a; // return 1;
}

_Bool test2_(int a, int b)
{
        a = g();
        int t = a;
        if (t < b) t = b;
        return t >= a; // return 1;
}

/* All of these should be optimized to just be the function calls and `return 1;` */
/* { dg-final { scan-tree-dump-times "return 1;" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not " MAX_EXPR " "optimized" } } */
