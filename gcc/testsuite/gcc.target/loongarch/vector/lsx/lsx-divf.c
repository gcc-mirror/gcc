/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mlsx -mfrecipe -fno-unsafe-math-optimizations" } */
/* { dg-final { scan-assembler "vfdiv.s" } } */
/* { dg-final { scan-assembler-not "vfrecipe.s" } } */

float a[4],b[4],c[4];

void
foo ()
{
  for (int i = 0; i < 4; i++)
    c[i] = a[i] / b[i];
}
