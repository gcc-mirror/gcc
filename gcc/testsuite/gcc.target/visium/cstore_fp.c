/* { dg-do compile } */
/* { dg-options "-O -fno-trapping-math" } */

int foo1 (float a, float b)
{
  return (a < b);
}

int foo2 (float a, float b)
{
  return (a > b);
}

int foo3 (float a, float b)
{
  return !(a < b);
}

int foo4 (float a, float b)
{
  return !(a > b);
}

/* { dg-final { scan-assembler-times "adc.l" 2 } } */
/* { dg-final { scan-assembler-times "subc.l" 2 } } */
