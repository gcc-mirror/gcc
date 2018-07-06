/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ch2-details" } */

int *a, *b;
int test(int n, int k)
{
  int it = 0;
  while (++it < n)
    {
      if (it % k == 1)
	a[it] = 0;
      else
	b[it] = 1;
    }
}

/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
