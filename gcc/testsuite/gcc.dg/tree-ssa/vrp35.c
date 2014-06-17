/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int test1(int i, int k)
{
  if (i > 0 && i <= 5 && k >= 10 && k < 42)
    {
      int j = i + 1 + k;
      return j == 10;
    }
  return 1;
}

/* { dg-final { scan-tree-dump "Folding predicate j_.* == 10 to 0" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
