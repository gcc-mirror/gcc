/* PR tree-optimization/94920 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-forwprop1" } */

typedef int __attribute__((vector_size(4*sizeof(int)))) vint;

/* Same form as PR.  */
__attribute__((noipa)) unsigned int foo(int x) {
    return (x >= 0 ? x : 0) + (x <= 0 ? -x : 0);
}

/* Test for forward propogation.  */
__attribute__((noipa)) unsigned int corge(int x) {
    int w = (x >= 0 ? x : 0);
    int y = -x;
    int z = (y >= 0 ? y : 0);
    return w + z;
}

/* Vector case.  */
__attribute__((noipa)) vint thud(vint x) {
    vint t = (x >= 0 ? x : 0) ;
    vint xx = -x;
    vint t1 =  (xx >= 0 ? xx : 0);
    return t + t1;
}

/* Signed function.  */
__attribute__((noipa)) int bar(int x) {
    return (x >= 0 ? x : 0) + (x <= 0 ? -x : 0);
}

/* Commutative property.  */
__attribute__((noipa)) unsigned int baz(int x) {
    return (x <= 0 ? -x : 0) + (x >= 0 ? x : 0);
}

/* Flipped order for max expressions.  */
__attribute__((noipa)) unsigned int quux(int x) {
    return (0 <= x ? x : 0) + (0 >= x ? -x : 0);
}

/* Not zero so should not optimize.  */
__attribute__((noipa)) unsigned int waldo(int x) {
    return (x >= 4 ? x : 4) + (x <= 4 ? -x : 4);
}

/* Not zero so should not optimize.  */
__attribute__((noipa)) unsigned int fred(int x) {
    return (x >= -4 ? x : -4) + (x <= -4 ? -x : -4);
}

/* Incorrect pattern.  */
__attribute__((noipa)) unsigned int goo(int x) {
    return (x <= 0 ? x : 0) + (x >= 0 ? -x : 0);
}

/* Incorrect pattern.  */
__attribute__((noipa)) int qux(int x) {
    return (x >= 0 ? x : 0) + (x >= 0 ? x : 0);
}

/* { dg-final {scan-tree-dump-times " ABS_EXPR " 6 "forwprop1" } } */
