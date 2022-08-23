/* PR tree-optimization/98304 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Signed test function.  */
__attribute__((noipa)) int foo(int n) {
    return n - (((n > 63) ? n : 63) & -64);
}

/* Unsigned test function.  */
__attribute__((noipa)) unsigned int bar(unsigned int n) {
    return n - (((n > 63) ? n : 63) & -64);
}

/* Different power of 2.  */
__attribute__((noipa)) int goo(int n) {
    return n - (((n > 31) ? n : 31) & -32);
}

/* Commutative property (should be identical to foo)  */
__attribute__((noipa)) int baz(int n) {
    return n - (((64 > n) ? 63 : n) & -64);
}

/* < instead of >.  */
__attribute__((noipa)) int fred(int n) {
    return n - (((63 < n) ? n : 63) & -64);
}

/* Constant is not a power of 2 so should not simplify.  */
__attribute__((noipa)) int qux(int n) {
    return n - (((n > 62) ? n : 62) & -63);
}

/* Constant is not a power of 2 so should not simplify.  */
__attribute__((noipa)) unsigned int quux(unsigned int n) {
    return n - (((n > 62) ? n : 62) & -63);
}

/* Constant is a variable so should not simplify.  */
__attribute__((noipa)) int waldo(int n, int x) {
    return n - (((n > 63) ? n : 63) & x);
}

/* Difference between constants is not -1.  */
__attribute__((noipa)) int corge(int n) {
    return n - (((n > 1) ? n : 1) & -64);
}

/* Difference between constants is not -1.  */
__attribute__((noipa)) unsigned int thud(unsigned int n)
{
    return n - (((n > 1) ? n : 1) & -64);
}

/* { dg-final { scan-tree-dump-times " - " 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= " 4 "optimized" } } */
