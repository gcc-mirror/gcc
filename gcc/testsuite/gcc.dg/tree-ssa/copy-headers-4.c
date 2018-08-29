/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ch2-details" } */

int *a, *b;
int test(int n, int k)
{
  int it = 0;
  do
    {
      if (it % k == 1)
	a[it] = 0;
      else
	b[it] = 1;
    }
  while (++it < n);
}

/* { dg-final { scan-tree-dump-not "is not do-while loop" "ch2" } } */
