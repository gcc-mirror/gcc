/* { dg-do compile } */
/* { dg-options "-O2 --param logical-op-non-short-circuit=1 -fdump-tree-optimized-raw" } */

/* PR tree-optimization/95929 */


static inline _Bool nand(_Bool a, _Bool b)
{
    return !(a && b);
}

_Bool f(int a, int b)
{
    return nand(nand(b, nand(a, a)), nand(a, nand(b, b)));
}

/* We should be able to optimize this to (a != 0) ^ (b != 0) */
/* There should be no negate_expr nor gimple_cond here. */

/* { dg-final { scan-tree-dump-not "negate_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-not "cond_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "gimple_assign " 3 "optimized" } } */
