/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "movl"} } */

int foo (int *a)
{
  int b = *a - 1;
  return b;
}

int foo2 (int a, int b)
{
  int c = a + b;
  return c;
}

int foo3 (int *a, int b)
{
  int c = *a + b;
  return c;
}
