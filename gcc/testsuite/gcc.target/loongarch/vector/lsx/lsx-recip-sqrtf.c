/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mlsx -mfrecipe" } */
/* { dg-final { scan-assembler-times "vfrsqrte.s" 3 } } */

float a[4], b[4], c[4];

extern float sqrtf (float);

void
foo1 (void)
{
  for (int i = 0; i < 4; i++)
    c[i] = a[i] / sqrtf (b[i]);
}

void
foo2 (void)
{
  for (int i = 0; i < 4; i++)
    c[i] = sqrtf (a[i] / b[i]);
}

void
foo3 (void)
{
  for (int i = 0; i < 4; i++)
    c[i] = sqrtf (a[i]);
}
