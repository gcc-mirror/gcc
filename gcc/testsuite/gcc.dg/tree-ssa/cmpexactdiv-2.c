/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int f (long *a, long *b, long *c) {
    __PTRDIFF_TYPE__ l1 = b - a;
    __PTRDIFF_TYPE__ l2 = c - a;
    return l1 < l2;
}

/* Eventually we also want to remove all minus_expr.  */
/* { dg-final { scan-tree-dump-not "exact_div_expr" "optimized" } } */
