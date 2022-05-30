/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mtune=skylake -mfpmath=sse" } */
/* { dg-final { scan-assembler-not "\\(%rsp\\)" } } */

static int as_int(float x)
{
    return (union{float x; int i;}){x}.i;
}

float f(double y, float x)
{
    int i = as_int(x);
    if (__builtin_expect(i > 99, 0)) return 0;
    if (i*2u < 77) if (i==2) return 0;
    return y*x;
}
