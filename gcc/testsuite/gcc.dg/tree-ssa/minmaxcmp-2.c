/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* PR tree-optimization/111364 */

#define min1(a, b) ((a) < (b) ? (a) : (b))
#define max1(a, b) ((a) > (b) ? (a) : (b))

int minlt(int a, int b)
{
        return min1(a, b) < a; // b < a or a > b
}
/* { dg-final { scan-tree-dump "return a > b;|return b < a;" "original" } } */

int minge(int c, int d)
{
        return min1(c, d) >= c; // d >= c or c <= d
}
/* { dg-final { scan-tree-dump "return c <= d;|return d <= c;" "original" } } */

int maxgt(int e, int f)
{
        return max1(e, f) > e; // e > f or f < e
}
/* { dg-final { scan-tree-dump "return e < f;|return f > e;" "original" } } */

int maxle(int x, int y)
{
        return max1(x, y) <= x; // y <= x or x >= y
}
/* { dg-final { scan-tree-dump "return x >= y;|return y <= x;" "original" } } */
