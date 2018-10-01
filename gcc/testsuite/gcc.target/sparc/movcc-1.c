/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v9" } */

int foo1 (int a)
{
  int b = a + 1;
  if (b != 0)
    return b;
  return 1;
}

int foo2 (int a)
{
  int b = a + 1;
  if (b < 0)
    return b;
  return 1;
}

int foo3 (int a)
{
  int b = a + 1;
  if (b >= 0)
    return b;
  return 1;
}

/* { dg-final { scan-assembler "move\t%"  } } */
/* { dg-final { scan-assembler "movpos\t%"  } } */
/* { dg-final { scan-assembler "movneg\t%" } } */
