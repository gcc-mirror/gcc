/* { dg-do compile } */
/* { dg-options "-O" } */

int foo1 (unsigned a, unsigned b)
{
  return (a < b);
}

int foo2 (unsigned a, unsigned b)
{
  return (a > b);
}

int foo3 (unsigned a, unsigned b)
{
  return (a >= b);
}

int foo4 (unsigned a, unsigned b)
{
  return (a <= b);
}

/* { dg-final { scan-assembler-times "adc.l" 2 } } */
/* { dg-final { scan-assembler-times "subc.l" 2 } } */
