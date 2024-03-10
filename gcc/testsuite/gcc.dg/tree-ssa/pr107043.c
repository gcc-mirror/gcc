// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

int g0(int n)
{
  int n1 = n & 0x8000;
  if (n1 == 0)
    return 1;
  // n1 will be 0x8000 here.
  return (n1 >> 15) & 0x1;
}

int g1(int n)
{
  int n1 = n & 0x8000;
  if (n1 == 0)
    return 1;
  // n>>15 will be xxxxxx1 here.
  return (n >> 15) & 0x1;
}

// { dg-final { scan-tree-dump-times "return 1;" 2 "evrp" } }
