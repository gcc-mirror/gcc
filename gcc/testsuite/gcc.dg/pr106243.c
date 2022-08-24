/* PR tree-optimization/106243 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-optimized" } */

#define vector __attribute__((vector_size(4*sizeof(int))))

/* Test from PR.  */
__attribute__((noipa)) int foo (int x) {
    return -x & 1;
}

/* Other test from PR.  */
__attribute__((noipa)) int bar (int x) {
    return (0 - x) & 1;
}

/* Forward propogation.  */
__attribute__((noipa)) int baz (int x) {
    x = -x;
    return x & 1;
}

/* Commutative property.  */
__attribute__((noipa)) int qux (int x) {
    return 1 & -x;
}

/* Vector test case.  */
__attribute__((noipa)) vector int waldo (vector int x) {
    return -x & 1;
}

/* Should not simplify.  */
__attribute__((noipa)) int thud (int x) {
    return -x & 2;
}

/* Should not simplify.  */
__attribute__((noipa)) int corge (int x) {
    return -x & -1;
}

/* { dg-final {scan-tree-dump-times "-" 2 "optimized" } } */
