/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -mtune=intel" } */
float f(float x)
{
    union {float f; unsigned i;} u = {x};
    u.i |= 0x80000000;
    return u.f;
}
/* { dg-final { scan-assembler-not "rsp"} } */
