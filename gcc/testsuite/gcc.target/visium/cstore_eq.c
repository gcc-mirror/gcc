/* { dg-do compile } */
/* { dg-options "-O" } */

int foo1 (int i)
{
  return (i != 0);
}

int foo2 (int i)
{
  return (i == 0);
}

int foo3 (int a, int b)
{
  return a != b;
}

int foo4 (int a, int b)
{
  return (a == b);
}

/* { dg-final { scan-assembler-times "adc.l" 2 } } */
/* { dg-final { scan-assembler-times "subc.l" 2 } } */
