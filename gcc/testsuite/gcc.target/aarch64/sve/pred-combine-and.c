/* { dg-do compile } */
/* { dg-options "-O3" } */

void f5(float * restrict z0, float * restrict z1, float *restrict x, float * restrict y, float c, int n)
{
    for (int i = 0; i < n; i++) {
        float a = x[i];
        float b = y[i];
        if (a > b) {
            z0[i] = a + b;
            if (a > c) {
                z1[i] = a - b;
            }
        }
    }
}

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-9]+/z, z[0-9]+\.s, z[0-9]+\.s} 2 } } */
