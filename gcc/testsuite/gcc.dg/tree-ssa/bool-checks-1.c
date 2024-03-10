/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/95923 */

_Bool f(_Bool a, _Bool b)
{
    if (!a && !b)
        return 0;
    if (!a && b)
        return 0;
    if (a && !b)
        return 0;
    return 1;
}

/* { dg-final { scan-tree-dump-times "bit_and_expr," 1 "optimized"} } */
/* { dg-final { scan-tree-dump-not   "bit_not_expr,"   "optimized"} } */
/* { dg-final { scan-tree-dump-not   "bit_ior_expr,"   "optimized"} } */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr,"   "optimized"} } */
/* { dg-final { scan-tree-dump-not   "eq_expr,"    "optimized"} } */
/* { dg-final { scan-tree-dump-not   "ne_expr,"    "optimized"} } */
/* { dg-final { scan-tree-dump-not   "gimple_cond"    "optimized"} } */
