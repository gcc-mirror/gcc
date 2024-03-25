/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mfrecipe -fno-unsafe-math-optimizations" } */
/* { dg-final { scan-assembler "fdiv.s" } } */
/* { dg-final { scan-assembler-not "frecipe.s" } } */

float
foo(float a, float b)
{
  return a / b;
}
