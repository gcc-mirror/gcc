/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x)
{
  return -(x == 0);
}

int bar(int x)
{
  int t = __builtin_clz(x);
  return -(t>>5);
}

/* { dg-final { scan-assembler-not "cntlzw" } } */
