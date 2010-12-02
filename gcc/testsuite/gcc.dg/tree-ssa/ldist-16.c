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

/* We should apply loop distribution and generate a memset (0).  */

/* { dg-final { scan-tree-dump "distributed: split to 2" "ldist" } } */
/* { dg-final { scan-tree-dump-times "__builtin_memset" 2 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
