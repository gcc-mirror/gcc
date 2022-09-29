// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

int g(int n)
{
  n &= 0x8000;
  if (n == 0)
    return 1;
  return __builtin_popcount(n);
}

// { dg-final { scan-tree-dump "return 1;" "evrp" } }
