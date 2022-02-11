/* { dg-do compile } */
/* { dg-options "-O3" } */

/*
** f11:
** ...
** 	ld1d	z0.d, p0/z, \[x1, x2, lsl 3\]
** 	fcmgt	p2.d, p3/z, z0.d, #0.0
** 	fcmgt	p1.d, p0/z, z0.d, #0.0
** 	not	p1.b, p0/z, p1.b
** 	ld1d	z1.d, p1/z, \[x3, x2, lsl 3\]
** ...
*/

void f11(double * restrict z, double * restrict w, double * restrict x, double * restrict y, int n)
{
    for (int i = 0; i < n; i++) {
        z[i] = (w[i] > 0) ? w[i] : y[i];
    }
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-times {\tnot\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.d, p[0-9]+/z, z[0-9]+\.d, #0.0} 1 } } */
