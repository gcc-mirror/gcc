/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-ompt=basic -fdump-tree-ompexp" } */

/* Check that an extra, OMPT variant of GOMP_loop_end is not emitted when the
   inscan modifier is present on the for construct.  */

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

/* { dg-final { scan-tree-dump "__builtin_GOMP_loop_end_nowait \\(" "ompexp" } } */
/* { dg-final { scan-tree-dump-not "__builtin_GOMP_loop_end_nowait \\(\\);\[\t\n \]*__builtin_GOMP_loop_static_worksharing_end \\(\\);" "ompexp" } } */
