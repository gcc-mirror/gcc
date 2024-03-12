/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* PR tree-optimization/103536 */

int
orand(int a, int b, int c)
{
    return (a | b) & (a & c); // a & c
}

/* { dg-final { scan-tree-dump "return a \& c;" "original" } } */

int
andor(int d, int e, int f)
{
    return (d & e) | (d | f); // d | f
}

/* { dg-final { scan-tree-dump "return d \\| f;" "original" } } */
