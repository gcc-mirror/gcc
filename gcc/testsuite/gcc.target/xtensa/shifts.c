/* { dg-do compile } */
/* { dg-options "-O1" } */

int lshift1(int v)
{
  return v << 1;
}

int lshift2(int v, int s)
{
  return v << (s * 8);
}

unsigned int lshift3(unsigned int v, int s)
{
  return v << (s * 8);
}

int rshift1(int v, int s)
{
  return v >> (s * 8);
}

unsigned int rshift2(unsigned int v, int s)
{
  return v >> (s * 8);
}

/* { dg-final { scan-assembler-not "slli" } } */
/* { dg-final { scan-assembler-times "ssa8l" 2 } } */
/* { dg-final { scan-assembler-times "ssa8b" 2 } } */
