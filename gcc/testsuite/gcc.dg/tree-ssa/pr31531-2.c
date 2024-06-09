/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/31531 */

int f0(unsigned x, unsigned t)
{
        x = ~x;
        t = ~t;
        int xx = x;
        int tt = t;
        return tt < xx;
}

int f1(unsigned x, int t)
{
        x = ~x;
        t = ~t;
        int xx = x;
        int tt = t;
        return tt < xx;
}

int f2(int x, unsigned t)
{
        x = ~x;
        t = ~t;
        int xx = x;
        int tt = t;
        return tt < xx;
}


/* We should be able to remove all ~ from the above functions. */
/* { dg-final { scan-tree-dump-times "~" 0 "optimized"} } */
