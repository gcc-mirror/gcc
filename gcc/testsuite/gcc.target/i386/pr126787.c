/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-options "-O2 -mapxf -mprefer-avx128 -funroll-loops --param max-unroll-times=4 -ffast-math -ftree-vectorize" } */

typedef float f4;
__attribute__((__target__("avx,fma4")))
void smm_avx128_fma4(f4 * restrict c, const f4 * restrict a,
                     const f4 * restrict b, int m, int n, int k)
{
    for (int j = 0; j < n; j++)
        for (int l = 0; l < k; l++) {
            f4 bl = b[l + j*k];
            for (int i = 0; i < m; i++)
                c[i + j*m] += a[i + l*m] * bl;
        }
}
