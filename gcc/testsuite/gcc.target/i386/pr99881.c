/* PR target/99881.  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -march=skylake" } */
/* { dg-final { scan-assembler-not "xmm\[0-9\]" { xfail *-*-* } } } */

void
foo (int* __restrict a, int n, int c)
{
    a[0] = n;
    a[1] = c;
}

void
foo1 (int* __restrict a, int n, int b, int c, int d)
{
    a[0] = n;
    a[1] = b;
    a[2] = c;
    a[3] = d;
}

void
foo2 (int* __restrict a, int n, int b, int c, int d, int e, int f, int g, int h)
{
    a[0] = n;
    a[1] = b;
    a[2] = c;
    a[3] = d;
    a[4] = e;
    a[5] = f;
    a[6] = g;
    a[7] = h;
}

void
foo3 (long long* __restrict a, long long n, long long c)
{
    a[0] = n;
    a[1] = c;
}

void
foo4 (long long* __restrict a, long long n, long long b, long long c, long long d)
{
    a[0] = n;
    a[1] = b;
    a[2] = c;
    a[3] = d;
}
