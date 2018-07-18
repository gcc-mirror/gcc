/* { dg-do compile } */
/* { dg-options "-O -frounding-math -fdump-tree-optimized-raw -Wno-psabi" } */

#define DEF(num, T1, T2) T2 f##num(T1 x) { \
    T1 y = -x; \
    T2 z = (T2)y; \
    return -z; \
}
DEF(0, int, long long)
DEF(1, int, unsigned long long)
DEF(2, long long, int)
DEF(3, unsigned long long, int)
DEF(4, long long, unsigned)
DEF(5, unsigned long long, unsigned)
DEF(6, float, double)

typedef int vec __attribute__((vector_size(4*sizeof(int))));
typedef unsigned uvec __attribute__((vector_size(4*sizeof(int))));
void h(vec*p,uvec*q){
    vec a = -*p;
    *q = -(uvec)a;
}

/* { dg-final { scan-tree-dump-not "negate_expr" "optimized"} } */
