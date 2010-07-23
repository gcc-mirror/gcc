/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse -mfpmath=sse -mrecip" } */

float a[16];
float b[16];
float r[16];

extern float sqrtf (float);

void t1(void)
{
 int i;

 for (i = 0; i < 16; i++)
   r[i] = a[i] / sqrtf (b[i]);
}

void t2(void)
{
 int i;

 for (i = 0; i < 16; i++)
   r[i] = sqrtf (a[i] / b[i]);
}

void t3(void)
{
 int i;

 for (i = 0; i < 16; i++)
   r[i] = sqrtf (a[i]);
}

/* { dg-final { scan-assembler-times "rsqrtps" 3 } } */
