/* { dg-do compile } */
/* { dg-options "-fsignaling-nans -fwrapv -fdump-tree-gimple" } */
int f(int i)
{
  return i < (i - 2);
}

int g(int i)
{
  return i > (i + 2);
}

int h (double i)
{
  return i >= i + 2.0;
}

int j (double i)
{
  return i > i + 2.0;
}
/* { dg-final { scan-tree-dump-times " = 0" 0 "gimple" } } */
