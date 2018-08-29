/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Go from four moves to two.  */

int
foo (long long x)
{
  return x <= 0x1999999999999998;
}

int
GT (unsigned int x)
{
  return x > 0xfefffffe;
}

int
LE (unsigned int x)
{
  return x <= 0xfefffffe;
}

int
GE (long long x)
{
  return x >= 0xff000000;
}

int
LT (int x)
{
  return x < 0xff000000;
}

/* Optimize the immediate in conditionals.  */

int
check (int x, int y)
{
  if (x > y && GT (x))
    return 100;

  return x;
}

int
tern (int x)
{
  return x >= 0xff000000 ? 5 : -3;
}

/* baz produces one movk instruction.  */
/* { dg-final { scan-assembler-times "movk" 1 } } */
