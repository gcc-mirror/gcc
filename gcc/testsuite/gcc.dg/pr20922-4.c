/* { dg-do compile } */
/* { dg-options "-fno-wrapv -fdump-tree-gimple" } */
int f(int i)
{
  return i < (i - 2);
}

int g(int i)
{
  return i > (i + 2);
}

int h(int i)
{
  return i < (i + (-2));
}

int j(int i)
{
  return i > (i - (-2));
}

int x(double i)
{
  return i < (i - 2.0);
}

int y(double i)
{
  return i > (i + 2.0);
}

int z(double i)
{
  return i < (i + (-2.0));
}
/* { dg-final { scan-tree-dump-times " = 0" 7 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
