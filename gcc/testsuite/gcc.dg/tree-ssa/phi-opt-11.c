/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt2 -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

int f(int a, int b, int c)
{
  if (a == 0 && b > c)
   return 0;
 return a;
}

int g(int a, int b, int c)
{
  if (a == 42 && b > c)
   return 42;
 return a;
}

int h(int a, int b, int c, int d)
{
  if (a == d && b > c)
   return d;
 return a;
}

int i(int a, int b, int c)
{
  if ((a > c) & (a == b))
    return a;
  return b;
}

int j(int a, int b, int c)
{
  if ((a > c) & (a == b))
    return b;
  return a;
}

int k(int a, int b, int c)
{
  if ((a > c) | (a != b))
    return b;
  return a;
}

int l(int a, int b, int c)
{
  if ((a > c) | (a != b))
    return a;
  return b;
}

/* { dg-final { scan-tree-dump-times "if" 0 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "if" 0 "optimized" } } */
