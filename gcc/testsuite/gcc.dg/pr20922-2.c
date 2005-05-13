/* { dg-do compile } */
/* { dg-options "-fwrapv -fdump-tree-gimple" } */
int f (int i)
{
  return (i - 2) > i;
}

int g (int i)
{
  return (i + 2) < i;
}

int h (double i)
{
  return (i + 2.0) <= i;
}
/* { dg-final { scan-tree-dump-times " = 0" 0 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
