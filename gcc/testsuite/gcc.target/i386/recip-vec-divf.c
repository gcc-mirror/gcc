/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse -mfpmath=sse -mrecip" } */

float a[16];
float b[16];
float r[16];

void t1(void)
{
 int i;

 for (i = 0; i < 16; i++)
   r[i] = a[i] / b[i];
}

/* { dg-final { scan-assembler "rcpps" } } */
