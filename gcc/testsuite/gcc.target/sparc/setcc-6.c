/* { dg-do compile } */
/* { dg-options "-O1" } */

int foo1 (int a, int i)
{
  return a + (i != 0);
}

int foo2 (int a, int i)
{
  return a - (i != 0);
}

int foo3 (int a, int b, int i)
{
  return a + b + (i != 0);
}

int foo4 (int a, int b, int i)
{
  return a - b - (i != 0);
}

int foo5 (int a, int i)
{
  return a + (i == 0);
}

int foo6 (int a, int i)
{
  return a - (i == 0);
}

/* { dg-final { scan-assembler-times "addx\t%" 3 } } */
/* { dg-final { scan-assembler-times "subx\t%" 3 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 6 } } */
/* { dg-final { scan-assembler-not "add\t%" } } */
/* { dg-final { scan-assembler-not "sub\t%" } } */
