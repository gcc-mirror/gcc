/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/109959 */

unsigned fu(unsigned a)
{
        _Bool t = a <= 1;
        return t & a;
}

_Bool fb(unsigned a)
{
        _Bool t = a <= 1;
        return t & a;
}

_Bool fb1(unsigned a)
{
        _Bool t = a <= 1;
        _Bool t1 = a;
        return t & t1;
}

signed fui(unsigned a)
{
        _Bool t = a <= 1;
        int ai = a;
        return t & ai;
}

/* These all should be optimized to `a == 1` */
/* { dg-final { scan-tree-dump-times "eq_expr," 4 "optimized"} } */
/* { dg-final { scan-tree-dump-not "le_expr," "optimized"} } */
/* { dg-final { scan-tree-dump-not "bit_and_expr," "optimized"} } */


