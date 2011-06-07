/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a" } */

long long foolong (long long x, short *a, short *b)
{
    return x + *a * *b;
}

/* { dg-final { scan-assembler "smlalbb" } } */
