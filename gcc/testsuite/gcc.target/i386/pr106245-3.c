/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

long long f(long long a)
{
    return (a << 63) >> 63;
}

/* { dg-final { scan-assembler "andl" } } */
/* { dg-final { scan-assembler "negl" } } */
/* { dg-final { scan-assembler "cltd" } } */
