/* PR middle-end/125708 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

int
foo (int a, _Bool b)
{
  return a / (2 - b);
}

int
foo1 (int a, _Bool b)
{
  return a / (4 + b);
}

int
foo2 (int a, _Bool b)
{
  return a / (8 - b);
}

unsigned
foo3 (unsigned a, _Bool b)
{
  return a / (4 + b);
}

int
foo4 (int a, _Bool b)
{
  return a / ((1 << 20) - b);
}

/* { dg-final { scan-assembler-not "idiv" } } */
/* { dg-final { scan-assembler-not "div" } } */
/* { dg-final { scan-assembler "cmov" } } */
