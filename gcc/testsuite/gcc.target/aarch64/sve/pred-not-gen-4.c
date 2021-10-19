/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <math.h>

void f13(double * restrict z, double * restrict w, double * restrict x, double * restrict y, int n)
{
    for (int i = 0; i < n; i++) {
        z[i] = (isunordered(w[i], 0)) ? x[i] + w[i] : y[i] - w[i];
    }
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-times {\tnot\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b\n} 1 } } */
