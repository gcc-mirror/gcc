/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx -mtune=generic -mfpmath=sse -mrecip" } */

float a[16];
float b[16];
float r[16];

void t1(void)
{
 int i;

 for (i = 0; i < 16; i++)
   r[i] = a[i] / b[i];
}

/* { dg-final { scan-assembler "vrcpps\[ \\t\]+\[^\n\]*%ymm" } } */
