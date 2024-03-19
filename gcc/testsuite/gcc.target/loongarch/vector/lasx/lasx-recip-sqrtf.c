/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mlasx -mfrecipe" } */
/* { dg-final { scan-assembler-times "xvfrsqrte.s" 3 } } */

float a[8], b[8], c[8];

extern float sqrtf (float);

void
foo1 (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = a[i] / sqrtf (b[i]);
}

void
foo2 (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = sqrtf (a[i] / b[i]);
}

void
foo3 (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = sqrtf (a[i]);
}
