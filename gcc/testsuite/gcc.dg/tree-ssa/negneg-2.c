/* { dg-do compile } */
/* { dg-options "-O -fno-rounding-math -fdump-tree-optimized-raw" } */

#define DEF(num, T1, T2) T2 f##num(T1 x) { \
    T1 y = -x; \
    T2 z = (T2)y; \
    return -z; \
}
DEF(0, double, float)

/* { dg-final { scan-tree-dump-not "negate_expr" "optimized"} } */
