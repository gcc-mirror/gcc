/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

int x[1000];

void foo (int n)
{
  int i;

  for (i = 0; i < n; ++i)
    {
      x[i] = 0;
      x[2*i + 1] = 1;
    }
}

/* We should not apply loop distribution and not generate a memset (0).  */

/* { dg-final { scan-tree-dump "Loop 1 is the same" "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 0 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
