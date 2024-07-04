/* { dg-do compile } */
/* { dg-options "-O2" } */

int f(int a)
{
    return (a << 31) >> 31;
}

/* { dg-final { scan-assembler "andl" } } */
/* { dg-final { scan-assembler "negl" } } */
