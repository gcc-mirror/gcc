/* PR tree-optimization/94920 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Form from PR.  */
__attribute__((noipa)) unsigned int foo(int x) {
    return x <= 0 ? -x : 0;
}

/* Changed order.  */
__attribute__((noipa)) unsigned int bar(int x) {
    return 0 >= x ? -x : 0;
}

/* { dg-final {scan-tree-dump-times " MAX_EXPR " 2 "optimized" } } */
