/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

/*
** f12:
** ...
** 	ld1w	z1.s, p0/z, \[x1, x2, lsl 2\]
** 	cmple	p1.s, p0/z, z1.s, #0
** 	ld1w	z0.s, p1/z, \[x3, x2, lsl 2\]
** ...
*/

void f12(int * restrict z, int * restrict w, int * restrict x, int * restrict y, int n)
{
    for (int i = 0; i < n; i++) {
        z[i] = (w[i] > 0) ? w[i] : y[i];
    }
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-not {\tnot\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b\n} } } */
