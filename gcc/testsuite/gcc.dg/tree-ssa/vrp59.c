/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fdump-tree-vrp1" } */

int f(int x)
{
  if (x >= 0 && x <= 3)
    {
      x = x ^ 3;
      x = x & 3;
    }
  return x;
}

int g(int x)
{
  if (x >= 0 && x <= 3)
    {
      x = x ^ 2;
      x = x & 3;
    }
  return x;
}

int h(int x)
{
  if (x >= 0 && x <= 3)
    {
      x = x ^ 1;
      x = x & 3;
    }
  return x;
}

/* { dg-final { scan-tree-dump-not " & 3;" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
