/* { dg-do compile } */
/* { dg-options "-O2" } */

long foo(long x)
{
  return -(x == 0);
}

long bar(long x)
{
  long t = __builtin_clzl(x);
  return -(t>>(sizeof(long) == 8 ? 6 : 5));
}

/* { dg-final { scan-assembler-not "cntlz" } } */
