/* PR tree-optimization/104992 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-optimized" } */

#define vector __attribute__((vector_size(4*sizeof(int))))

/* Form from PR.  */
__attribute__((noipa)) unsigned foo(unsigned x, unsigned y)
{
    return x / y * y == x;
}

__attribute__((noipa)) unsigned bar(unsigned x, unsigned y) {
    return x == x / y * y;
}

/* Signed test case.  */
__attribute__((noipa)) unsigned baz (int x, int y) {
    return x / y * y == x;
}

/* Changed order.  */
__attribute__((noipa)) unsigned qux (unsigned x, unsigned y) {
    return y * (x / y) == x;
}

/* Test for forward propogation.  */
__attribute__((noipa)) unsigned corge(unsigned x, unsigned y) {
    int z = x / y;
    int q = z * y;
    return q == x; 
}

/* Test vector case.  */
__attribute__((noipa)) vector int thud(vector int x, vector int y) {
    return x / y * y == x;
}

/* Complex type should not simplify because mod is different.  */
__attribute__((noipa)) int goo(_Complex int x, _Complex int y)
{
    _Complex int z = x / y;
    _Complex int q = z * y;
    return q == x; 
}

/* Wrong order.  */
__attribute__((noipa)) unsigned fred (unsigned x, unsigned y) {
    return y * x / y == x;
}

/* Wrong pattern.  */
__attribute__((noipa)) unsigned waldo (unsigned x, unsigned y, unsigned z) {
    return x / y * z == x;
}

/* { dg-final { scan-tree-dump-times " % " 9 "optimized" { target { ! vect_int_mod } } } } */
/* { dg-final { scan-tree-dump-times " % " 6 "optimized" { target vect_int_mod } } } */
