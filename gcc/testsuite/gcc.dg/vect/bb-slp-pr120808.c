/* { dg-do compile } */
/* { dg-additional-options "-ffp-contract=on" } */
/* { dg-additional-options "-mfma -mfpmath=sse" { target { x86_64-*-* i?86-*-* } } } */

void f(double x[restrict], double *y, double *z)
{
    x[0] = x[0] * y[0] + z[0];
    x[1] = x[1] * y[1] - z[1];
}

/* The following should check for SLP build covering the loads.  */
/* { dg-final { scan-tree-dump "Found VEC_FMSUBADD pattern" "slp2" { target { x86_64-*-* i?86-*-* } } } } */
