/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx -mtune=generic -mfpmath=sse -mrecip" } */

float a[32];
float b[32];
float r[32];

void t1(void)
{
 int i;

 for (i = 0; i < 32; i++)
   r[i] = a[i] / b[i];
}

/* { dg-final { scan-assembler "vrcpps\[ \\t\]+\[^\n\]*%ymm" } } */
