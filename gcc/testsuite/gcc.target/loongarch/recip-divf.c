/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mfrecipe" } */
/* { dg-final { scan-assembler "frecipe.s" } } */

float
foo(float a, float b)
{
  return a / b;
}
