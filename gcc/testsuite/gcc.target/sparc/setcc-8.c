/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -mno-vis3 -mno-subxc" } */

long foo1 (long a, int i)
{
  return a + (i != 0);
}

long foo2 (long a, int i)
{
  return a - (i != 0);
}

long foo3 (long a, long b, int i)
{
  return a + b + (i != 0);
}

long foo4 (long a, long b, int i)
{
  return a - b - (i != 0);
}

long foo5 (long a, int i)
{
  return a + (i == 0);
}

long foo6 (long a, int i)
{
  return a - (i == 0);
}

/* { dg-final { scan-assembler-times "addx\t%" 3 } } */
/* { dg-final { scan-assembler-times "subx\t%" 3 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 6 } } */
/* { dg-final { scan-assembler-not "add\t%" } } */
/* { dg-final { scan-assembler-not "sub\t%" } } */
