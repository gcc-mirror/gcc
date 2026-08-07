/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-ompexp" } */

/* Check that the start variant of GOMP_loop_static_worksharing is emitted
   when the inscan modifier is present on the for construct.  */

#define N 100

void f(void) {
    int a[N], b[N];
    int x = 0;

#pragma omp parallel for simd reduction(inscan, +: x)
    for (int k = 0; k < N; k++) {
        x += a[k];
#pragma omp scan inclusive(x)
        b[k] = x;
    }
}

/* { dg-final { scan-tree-dump "__builtin_GOMP_loop_static_worksharing_start \\(" "ompexp" } } */
