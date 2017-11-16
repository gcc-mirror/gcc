/* { dg-do compile } */
/* { dg-options "-O -frounding-math -fdump-tree-optimized-raw" } */

// This assumes that long long is strictly larger than int

#define DEF(num, T1, T2) T2 f##num(T1 x) { \
    T1 y = -x; \
    T2 z = (T2)y; \
    return -z; \
}
DEF(0, unsigned, long long)
DEF(1, unsigned, unsigned long long)
DEF(2, double, float)

/* { dg-final { scan-tree-dump-times "negate_expr" 6 "optimized"} } */
