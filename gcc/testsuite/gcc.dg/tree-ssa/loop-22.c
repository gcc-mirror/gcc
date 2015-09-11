/* { dg-options "-O2 -fdump-tree-optimized" } */

int a[100];

void test (int n)
{
  int i;

  for (i = 0; i < n; i += 3)
    a[i] = i;
}

/* We used to replace the exit test "i < n" by "i != ((n-1)/3) * 3 + 1".  Although
   correct, this transformation is obviously harmful.  */

/* { dg-final { scan-tree-dump-times "/" 0 "optimized" } } */
