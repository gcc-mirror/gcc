/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mdiv32" } */
/* { dg-final { scan-assembler-not "slli\\.w" } } */

int
f1 (int a, int b)
{
  return (a << b) | b;
}

int
f2 (int a, int b)
{
  return (a - b) | b;
}

int
f3 (int a, int b)
{
  return (a * b) | b;
}

int
f4 (int a, int b)
{
  return (unsigned) a >> b | (unsigned) a << (32 - b) | b;
}

int
f5 (int a, int b)
{
  return (unsigned) a << b | (unsigned) a >> (32 - b) | b;
}

int
f6 (int a, int b)
{
  return (a % b) | b;
}

int
f7 (int a, int b)
{
  return (a + b) | b;
}
