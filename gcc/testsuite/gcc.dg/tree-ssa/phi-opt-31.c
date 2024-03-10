/* PR tree-optimization/94898 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */


_Bool f(int x, int y)
{
    if (x >= y)
        return x - y;
    return 0;
}

/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-times "gt_expr," 1 "optimized"  } } */
/* { dg-final { scan-tree-dump-not "ne_expr," "optimized"  } } */
