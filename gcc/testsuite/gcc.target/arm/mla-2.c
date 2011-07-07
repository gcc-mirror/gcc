/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O2" } */

long long foolong (long long x, short *a, short *b)
{
    return x + *a * *b;
}

/* { dg-final { scan-assembler "smlalbb" } } */
