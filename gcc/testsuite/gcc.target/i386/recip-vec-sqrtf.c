/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse -mfpmath=sse -mrecip" } */

float a[4];
float b[4];
float r[4];

extern float sqrtf (float);

void t1(void)
{
 int i;

 for (i = 0; i < 4; i++)
   r[i] = a[i] / sqrtf (b[i]);
}

void t2(void)
{
 int i;

 for (i = 0; i < 4; i++)
   r[i] = sqrtf (a[i] / b[i]);
}

void t3(void)
{
 int i;

 for (i = 0; i < 4; i++)
   r[i] = sqrtf (a[i]);
}

/* { dg-final { scan-assembler-times "rsqrtps" 3 } } */
