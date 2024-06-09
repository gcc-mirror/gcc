/* { dg-do compile } */
/* { dg-options "-O2" } */

int f(int a)
{
    return (a << 31) >> 31;
}

/* { dg-final { scan-assembler-not "sarb" } } */
/* { dg-final { scan-assembler-not "movsbl" } } */
