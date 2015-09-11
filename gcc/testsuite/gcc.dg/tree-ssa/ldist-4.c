/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  unsigned int j;
  int a[100], b[100][100];

  a[0] = k;
  for (i = 1; i < 100; i ++)
    {
      for (j = 1; j < 100; j++)
	{
	  a[j] = k * i;
	  b[i][j] = a[j-1] + k;
	}
    }

  return b[100-1][1];
}

/* The current cost model fuses the two partitions because they have
   similar memory accesses.  */
/* { dg-final { scan-tree-dump "similar memory accesses" "ldist" } } */
/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
