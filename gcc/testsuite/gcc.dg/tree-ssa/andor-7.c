/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* PR tree-optimization/98710 */

signed foo(signed x, signed y, signed z)
{
    return (x | z) & ~(y | z); // x & ~(y | z);
}
// Note . here is `(` or `)`
/* { dg-final { scan-tree-dump "return x \& ~.y \\| z.;|return ~.y \\| z. \& x;" "original" } } */

signed foo_or(signed a, signed b, signed c)
{
    return (a & c) | ~(b & c); // a | ~(b & c);
}
/* { dg-final { scan-tree-dump "return a \\| ~.b \& c.;|return ~.b \& c. \\| a;" "original" } } */
