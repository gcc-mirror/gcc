/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/103536 */

_Bool
src_1 (_Bool a, _Bool b)
{
    return (a || b) && (a && b);
}

/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. \& b_\[0-9\]+.D." "optimized" } } */
/* { dg-final { scan-tree-dump-not "a_\[0-9\]+.D. \\\| b_\[0-9\]+.D." "optimized" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
