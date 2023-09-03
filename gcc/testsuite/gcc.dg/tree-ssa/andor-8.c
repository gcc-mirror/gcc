/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* PR tree-optimization/98710 */

signed foo2(signed a, signed b, signed c)
{
    return (a & ~(b | a)) & c; // 0
}
/* { dg-final { scan-tree-dump "return 0;" "original" } } */
signed foo2_or(signed x, signed y, signed z)
{
    return (x | ~(y & x)) & z; // -1 & z -> z
}

/* { dg-final { scan-tree-dump "return z;" "original" } } */
/* All | and & should have been removed. */
/* { dg-final { scan-tree-dump-not "~" "original" } } */
/* { dg-final { scan-tree-dump-not " \& " "original" } } */
/* { dg-final { scan-tree-dump-not " \\| " "original" } } */
