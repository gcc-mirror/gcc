/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_float } */

typedef float real_t;
__attribute__((aligned(64))) real_t a[32000], b[32000], c[32000];
real_t s482()
{
    for (int nl = 0; nl < 10000; nl++) {
        for (int i = 0; i < 32000; i++) {
            a[i] += b[i] * c[i];
            if (c[i] > b[i]) break;
        }
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
