/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse -mfpmath=sse -mrecip -fno-common" } */

float a[4];
float b[4];
float r[4];

void t1(void)
{
 int i;

 for (i = 0; i < 4; i++)
   r[i] = a[i] / b[i];
}

/* { dg-final { scan-assembler "rcpps" } } */
