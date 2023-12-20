/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test for wchar_t type.  */
int
main(void)
{
  int i;
  wchar_t a[100], s;

#pragma acc parallel reduction (+:s)
  for (i = 0; i < 10; i++)
    {
      s += a[i];
    }

#pragma acc serial reduction (+:s)
  for (i = 0; i < 10; i++)
    {
      s += a[i];
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "map\\(tofrom:s \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
