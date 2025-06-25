/* { dg-do compile } */
/* { dg-additional-options "-mfma" { target { x86_64-*-* i?86-*-* } } } */

double f(double x[], long n)
{
    double r0 = 0, r1 = 0;
    for (; n; x += 2, n--) {
        r0 = __builtin_fma(x[0], x[0], r0);
        r1 = __builtin_fma(x[1], x[1], r1);
    }
    return r0 + r1;
}

/* We should vectorize this as SLP reduction.  */
/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors and unroll factor 1" "vect" { target { x86_64-*-* i?86-*-* } } } } */
