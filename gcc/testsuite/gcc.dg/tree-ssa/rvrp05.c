/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-rvrp-details" } */

int test1(int i, int k)
{
  if (i > 0 && i <= 5 && k >= 10 && k < 42)
    {
      int j = i + 1 + k;
      if (j == 10)
        return 0;
    }
  return 1;
}

/* { dg-final { scan-tree-dump "Branch rewritten" "rvrp" } } */
