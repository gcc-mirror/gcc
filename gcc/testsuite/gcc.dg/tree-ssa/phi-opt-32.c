/* PR tree-optimization/99069 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

_Bool f(_Bool x, _Bool y)
{
    return (x ? y : 0) ? x : 0;
}


/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr," 1 "optimized"  } } */
