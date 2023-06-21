/* { dg-do compile } */
/* { dg-options "-O3" } */

void f13(double * restrict z, double * restrict w, double * restrict x, double * restrict y, int n)
{
    for (int i = 0; i < n; i++) {
        z[i] = (__builtin_isunordered(w[i], 0)) ? x[i] + w[i] : y[i] - w[i];
    }
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-times {\tnot\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.d, p[0-9]+/z, z[0-9]+\.d, z[0-9]+\.d} 1 } } */
