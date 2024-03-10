/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/110937 */

_Bool f2(_Bool a, _Bool b)
{
        if (a)
          return !b;
        return b;
}

/* We should be able to remove the conditional and convert it to an xor. */
/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 1 "optimized" } } */
