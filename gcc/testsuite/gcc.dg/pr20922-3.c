/* { dg-do compile } */
/* { dg-options "-ffast-math -fno-wrapv -fstrict-overflow -fdump-tree-gimple" } */
int f(int i)
{
  return (i - 2) <= i;
}

int g(int i)
{
  return (i + 2) >= i;
}

int h(int i)
{
  return (i + (-2)) <= i;
}

int x(double i)
{
  return (i - 2.0) <= i;
}

int y(double i)
{
  return (i + 2.0) >= i;
}

int z(double i)
{
  return (i + (-2.0)) <= i;
}
/* { dg-final { scan-tree-dump-times " = 1" 6 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
