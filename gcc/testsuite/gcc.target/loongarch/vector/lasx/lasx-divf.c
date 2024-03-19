/* { dg-do compile } */
/* { dg-options "-O2 -mrecip -mlasx -mfrecipe -fno-unsafe-math-optimizations" } */
/* { dg-final { scan-assembler "xvfdiv.s" } } */
/* { dg-final { scan-assembler-not "xvfrecipe.s" } } */

float a[8],b[8],c[8];

void 
foo ()
{
  for (int i = 0; i < 8; i++)
    c[i] = a[i] / b[i];
}
