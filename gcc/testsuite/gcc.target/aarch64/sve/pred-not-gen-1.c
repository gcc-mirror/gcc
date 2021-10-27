/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

/*
** f10:
** ...
** 	ld1d	z1.d, p0/z, \[x1, x5, lsl 3\]
** 	fcmgt	p2.d, p0/z, z1.d, #0.0
** 	ld1d	z2.d, p2/z, \[x2, x5, lsl 3\]
** 	not	p1.b, p0/z, p2.b
** 	ld1d	z0.d, p1/z, \[x3, x5, lsl 3\]
** ...
*/

void f10(double * restrict z, double * restrict w, double * restrict x, double * restrict y, int n)
{
    for (int i = 0; i < n; i++) {
        z[i] = (w[i] > 0) ? x[i] + w[i] : y[i] - w[i];
    }
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-times {\tnot\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b\n} 1 } } */
